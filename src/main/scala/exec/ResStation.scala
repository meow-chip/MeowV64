package exec
import core.CoreDef
import chisel3._
import chisel3.util.MuxLookup
import chisel3.util.log2Ceil
import chisel3.util.MuxCase
import instr.Decoder
import cache.DCFenceStatus

class ResStationExgress(implicit val coredef: CoreDef) extends Bundle {
  val instr = Output(new ReservedInstr)
  val valid = Output(Bool())
  val pop = Input(Bool())
}

trait ResStation {
  // Sorry, I was unable to come up with a good naming for the ports
  val ingress: Bundle {
    val instr: ReservedInstr

    /**
     * This is not a simple decoupled valid-ready handshake
     * 
     * free should always be asserted before commit, because the issuer may need
     * to decide between multiple applicable ready exec units
     */
    val free: Bool
    val push: Bool
  }

  val exgress: ResStationExgress

  val cdb: CDB
  val ctrl: Bundle {
    val flush: Bool
  }
}

/**
 * Out-of-Order Reservation station
 * 
 * For every cycle, only one instr maybe issued into this station,
 * and only one ready instr may start executing
 */
class OoOResStation(val idx: Int)(implicit val coredef: CoreDef) extends MultiIOModule with ResStation {

  val DEPTH = coredef.RESERVATION_STATION_DEPTHS(idx)

  val ingress = IO(new Bundle {
    val instr = Input(new ReservedInstr)
    val free = Output(Bool())
    val push = Input(Bool())
  })

  val exgress = IO(new ResStationExgress)

  val cdb = IO(Input(new CDB))
  val ctrl = IO(new Bundle {
    val flush = Input(Bool())
  })

  val store = RegInit(VecInit(Seq.fill(DEPTH)(ReservedInstr.empty)))
  val occupied = RegInit(VecInit(Seq.fill(DEPTH)(false.B)))

  val defIdx = Wire(UInt(log2Ceil(DEPTH).W))
  defIdx := DontCare

  // Exgress part
  exgress.valid := store.zip(occupied).foldLeft(false.B)((acc, grp) => grp match {
    case (instr, valid) => acc || (valid && instr.ready)
  })
  // MuxCase can handle multiple enabled cases
  val exgIdx = MuxCase(
    defIdx,
    store.zip(occupied).zipWithIndex.map({
      case ((instr, valid), idx) => (valid && instr.ready, idx.U(log2Ceil(DEPTH).W))
    })
  )
  exgress.instr := store(exgIdx)
  when(exgress.pop) {
    occupied(exgIdx) := false.B
  }

  // CDB data fetch
  for(instr <- store) {
    // Later entries takes priority
    for(ent <- cdb.entries) {
      when(ent.name =/= 0.U && ent.name === instr.rs1name && ent.valid) {
        // > This cannot happen because we limit the inflight instr count,
        // > so that reg names should not wrap around for in-flight instrs

        // This is not true anomore, because we can refer to previous tags used by a reg, that haven't been
        // re-assigned to another instruction

        // assert(!instr.rs1ready)
        when(!instr.rs1ready) {
          instr.rs1ready := true.B
          instr.rs1val := ent.data
        }
      }

      when(ent.name =/= 0.U && ent.name === instr.rs2name && ent.valid) {
        // assert(!instr.rs2ready)
        when(!instr.rs2ready) {
          instr.rs2ready := true.B
          instr.rs2val := ent.data
        }
      }
    }
  }

  // Ingress part
  //
  // Placed after CDB fetch to avoid being overwritten after a flush,
  // when a pending instruction may lies in the store
  ingress.free := occupied.foldLeft(false.B)((acc, valid) => acc || !valid)
  val ingIdx = MuxCase(
    defIdx,
    occupied.zipWithIndex.map({
      case (valid, idx) => (!valid, idx.U(log2Ceil(DEPTH).W))
    })
  )
  when(ingress.push) {
    occupied(ingIdx) := true.B
    store(ingIdx) := ingress.instr
  }

  assert(!(
    ingress.push &&
    exgress.pop &&
    ingIdx === exgIdx
  ))

  when(ctrl.flush) {
    // We don't need to reset store
    // store := VecInit(Seq.fill(DEPTH)(ReservedInstr.empty))
    occupied := VecInit(Seq.fill(DEPTH)(false.B))
  }
}


/**
 * Load-Store Buffer
 * 
 * Instructions are executed in-order, so effects of all memory operations
 * become visible to the core itself in program order
 * 
 * L1 may do RAW and WAW reordering, so the effect may not be in program order for other cores
 */
class LSBuf(val idx: Int)(implicit val coredef: CoreDef) extends MultiIOModule with ResStation {

  val DEPTH = coredef.RESERVATION_STATION_DEPTHS(idx)

  val ingress = IO(new Bundle {
    val instr = Input(new ReservedInstr)
    val free = Output(Bool())
    val push = Input(Bool())
  })

  val exgress = IO(new ResStationExgress)

  val fs = IO(new DCFenceStatus(coredef.L1D))

  val cdb = IO(Input(new CDB))
  val ctrl = IO(new Bundle {
    val flush = Input(Bool())
  })

  val hasPending = IO(Input(Bool())) // Is there any unfinished pending memory operations?

  val store = RegInit(VecInit(Seq.fill(DEPTH)(ReservedInstr.empty)))

  val head = RegInit(0.U(log2Ceil(DEPTH).W))
  val tail = RegInit(0.U(log2Ceil(DEPTH).W))

  assume((DEPTH & (DEPTH-1)) == 0)

  // Exgress part
  // Extra restrictions: no pending writes

  val headIsLoad = store(head).instr.instr.op === Decoder.Op("LOAD").ident
  val headIsFence = store(head).instr.instr.op === Decoder.Op("MEM-MISC").ident
  // TODO: optimize: allow stores with different address to slip over?
  val loadBlocked = hasPending
  val fenceBlocked = hasPending || !fs.wbufClear
  val instrReady = head =/= tail && store(head).ready
  when(headIsFence) {
    exgress.valid := instrReady && !fenceBlocked
  }.elsewhen(headIsLoad) {
    exgress.valid := instrReady && !loadBlocked
  }.otherwise {
    exgress.valid := instrReady
  }

  exgress.instr := store(head)
  when(exgress.pop) {
    head := head +% 1.U
  }

  // CDB data fetch
  for(instr <- store) {
    // Later entries takes priority
    for(ent <- cdb.entries) {
      when(ent.name =/= 0.U && ent.name === instr.rs1name && ent.valid) {
        // > This cannot happen because we limit the inflight instr count,
        // > so that reg names should not wrap around for in-flight instrs

        // This is not true anomore, because we can refer to previous tags used by a reg, that haven't been
        // re-assigned to another instruction

        // assert(!instr.rs1ready)
        when(!instr.rs1ready) {
          instr.rs1ready := true.B
          instr.rs1val := ent.data
        }
      }

      when(ent.name =/= 0.U && ent.name === instr.rs2name && ent.valid) {
        // assert(!instr.rs2ready)
        when(!instr.rs2ready) {
          instr.rs2ready := true.B
          instr.rs2val := ent.data
        }
      }
    }
  }

  // Ingress part
  ingress.free := tail +% 1.U =/= head
  when(ingress.push) {
    store(tail) := ingress.instr
    tail := tail +% 1.U
  }

  // Flush
  when(ctrl.flush) {
    head := 0.U
    tail := 0.U
  }
}
