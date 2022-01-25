package meowv64.exec
import chisel3._
import chisel3.util.Decoupled
import chisel3.util.PriorityEncoder
import chisel3.util.log2Ceil
import meowv64.cache.DCFenceStatus
import meowv64.core.CoreDef
import meowv64.instr.Decoder
import meowv64.util.FlushableSlot

class ResStationEgress(implicit val coredef: CoreDef) extends Bundle {
  val instr = Decoupled(new ReservedInstr)
}

trait ResStation {
  // Sorry, I was unable to come up with a good naming for the ports
  val ingress: Bundle {
    val instr: ReservedInstr

    /** This is not a simple decoupled valid-ready handshake
      *
      * free should always be asserted before commit, because the issuer may
      * need to decide between multiple applicable ready exec units
      */
    val free: Bool
    val push: Bool
  }

  val egress: ResStationEgress

  val cdb: CDB
  val ctrl: Bundle {
    val flush: Bool
  }
}

/** Out-of-Order Reservation station
  *
  * For every cycle, only one instr maybe issued into this station, and only one
  * ready instr may start executing
  */
class OoOResStation(val idx: Int)(implicit val coredef: CoreDef)
    extends Module
    with ResStation {

  val DEPTH = coredef.RESERVATION_STATION_DEPTHS(idx)

  val ingress = IO(new Bundle {
    val instr = Input(new ReservedInstr)
    val free = Output(Bool())
    val push = Input(Bool())
  })

  val egress = IO(new ResStationEgress)

  val cdb = IO(Input(new CDB))
  val ctrl = IO(new Bundle {
    val flush = Input(Bool())
  })

  val store = RegInit(VecInit(Seq.fill(DEPTH)(ReservedInstr.empty)))
  val occupied = RegInit(VecInit(Seq.fill(DEPTH)(false.B)))

  val defIdx = Wire(UInt(log2Ceil(DEPTH).W))
  defIdx := DontCare

  // Egress part
  val egMask = WireDefault(
    VecInit(
      store.zip(occupied).map({ case (instr, valid) => valid && instr.ready })
    )
  )
  val egIdx = PriorityEncoder(egMask)
  val maskedStore = WireDefault(store)

  val egSlot = Module(new FlushableSlot(new ReservedInstr(), true, false))

  egress.instr.valid := egSlot.io.deq.valid
  // zero when invalid
  when(egSlot.io.deq.valid) {
    egress.instr.bits := egSlot.io.deq.bits
  } otherwise {
    egress.instr.bits := 0.U.asTypeOf(egress.instr.bits)
  }

  egSlot.io.deq.ready := egress.instr.ready

  egSlot.io.enq.bits := maskedStore(egIdx)
  egSlot.io.enq.valid := egMask.asUInt().orR
  when(egSlot.io.enq.fire) {
    occupied(egIdx) := false.B
  }

  // CDB data fetch
  for ((instr, idx) <- store.zipWithIndex) {
    // Later entries takes priority
    for (ent <- cdb.entries) {
      when(ent.name === instr.rs1name && ent.valid) {
        // > This cannot happen because we limit the inflight instr count,
        // > so that reg names should not wrap around for in-flight instrs

        // This is not true anymore, because we can refer to previous tags used by a reg, that haven't been
        // re-assigned to another instruction

        // assert(!instr.rs1ready)
        when(!instr.rs1ready) {
          instr.rs1ready := true.B
          instr.rs1val := ent.data
          maskedStore(idx).rs1val := ent.data
        }

        egMask(idx) := occupied(idx) && instr.rs2ready && instr.rs3ready
      }

      when(ent.name === instr.rs2name && ent.valid) {
        // assert(!instr.rs2ready)
        when(!instr.rs2ready) {
          instr.rs2ready := true.B
          instr.rs2val := ent.data
          maskedStore(idx).rs2val := ent.data
        }

        egMask(idx) := occupied(idx) && instr.rs1ready && instr.rs3ready
      }

      when(ent.name === instr.rs3name && ent.valid) {
        // assert(!instr.rs3ready)
        when(!instr.rs3ready) {
          instr.rs3ready := true.B
          instr.rs3val := ent.data
          maskedStore(idx).rs3val := ent.data
        }

        egMask(idx) := occupied(idx) && instr.rs1ready && instr.rs2ready
      }
    }
  }

  // Ingress part
  //
  // Placed after CDB fetch to avoid being overwritten after a flush,
  // when a pending instruction may lies in the store
  val freeMask = occupied.map(!_)
  ingress.free := VecInit(freeMask).asUInt().orR
  val ingIdx = PriorityEncoder(freeMask)

  when(ingress.push) {
    occupied(ingIdx) := true.B
    store(ingIdx) := ingress.instr
  }

  assert(
    !(
      ingress.push &&
        egSlot.io.enq.fire &&
        ingIdx === egIdx
    )
  )

  assert(
    !(
      egress.instr.ready && !egSlot.io.deq.valid
    )
  )

  egSlot.io.flush.get := ctrl.flush

  when(ctrl.flush) {
    // We don't need to reset store
    // store := VecInit(Seq.fill(DEPTH)(ReservedInstr.empty))
    occupied := VecInit(Seq.fill(DEPTH)(false.B))
  }
}

/** Load-Store Buffer
  *
  * Instructions are executed in-order, so effects of all memory operations
  * become visible to the core itself in program order
  *
  * L1 may do RAW and WAW reordering, so the effect may not be in program order
  * for other cores
  */
class LSBuf(val idx: Int)(implicit val coredef: CoreDef)
    extends Module
    with ResStation {

  val DEPTH = coredef.RESERVATION_STATION_DEPTHS(idx)

  val ingress = IO(new Bundle {
    val instr = Input(new ReservedInstr)
    val free = Output(Bool())
    val push = Input(Bool())
  })

  val egress = IO(new ResStationEgress)

  val fs = IO(new DCFenceStatus(coredef.L1D))

  val cdb = IO(Input(new CDB))
  val ctrl = IO(new Bundle {
    val flush = Input(Bool())
  })

  /** Are there any unfinished pending memory operations? */
  val hasPending = IO(
    Input(Bool())
  )

  val store = RegInit(VecInit(Seq.fill(DEPTH)(ReservedInstr.empty)))

  val head = RegInit(0.U(log2Ceil(DEPTH).W))
  val tail = RegInit(0.U(log2Ceil(DEPTH).W))

  // power of 2
  assume((DEPTH & (DEPTH - 1)) == 0)

  // Egress part
  // Extra restrictions: no pending writes

  val headIsLoad = (
    store(head).instr.instr.op === Decoder.Op("LOAD").ident
      || store(head).instr.instr.op === Decoder.Op("LOAD-FP").ident
      || (store(head).instr.instr.op === Decoder.Op("AMO").ident
        && store(head).instr.instr.funct7(6, 2) === Decoder.AMO_FUNC("LR"))
  )
  val headIsFence = (
    store(head).instr.instr.op === Decoder.Op("MISC-MEM").ident
    // Release ops cannot be reordered before any previous ops
      || (store(head).instr.instr.op === Decoder.Op("AMO").ident
        && store(head).instr.instr.funct7(1))
  )

  // FIXME: are there acquire ops dispatched?

  // TODO: optimize: allow stores with different address to slip over?
  val loadBlocked = hasPending
  val fenceBlocked = hasPending || !fs.wbufClear
  val instrReady = head =/= tail && store(head).ready
  when(headIsFence) {
    egress.instr.valid := instrReady && !fenceBlocked
  }.elsewhen(headIsLoad) {
    egress.instr.valid := instrReady && !loadBlocked
  }.otherwise {
    egress.instr.valid := instrReady
  }

  egress.instr.bits := store(head)
  when(egress.instr.fire) { // FIXME: check egress.valid on regular ResStation
    head := head +% 1.U
  }

  // CDB data fetch
  for (instr <- store) {
    // Later entries takes priority
    for (ent <- cdb.entries) {
      when(ent.name === instr.rs1name && ent.valid) {
        // > This cannot happen because we limit the inflight instr count,
        // > so that reg names should not wrap around for in-flight instrs

        // This is not true anymore, because we can refer to previous tags used by a reg, that haven't been
        // re-assigned to another instruction

        // assert(!instr.rs1ready)
        when(!instr.rs1ready) {
          instr.rs1ready := true.B
          instr.rs1val := ent.data
        }
      }

      when(ent.name === instr.rs2name && ent.valid) {
        // assert(!instr.rs2ready)
        when(!instr.rs2ready) {
          instr.rs2ready := true.B
          instr.rs2val := ent.data
        }
      }

      when(ent.name === instr.rs3name && ent.valid) {
        // assert(!instr.rs3ready)
        when(!instr.rs3ready) {
          instr.rs3ready := true.B
          instr.rs3val := ent.data
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
