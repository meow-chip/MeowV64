package meowv64.exec

import chisel3._
import chisel3.util.Mux1H
import chisel3.util.log2Ceil
import meowv64.core.CSRWriter
import meowv64.core.CoreDef
import meowv64.core.PrivLevel
import meowv64.core.Status
import meowv64.exec.UnitSel.Retirement
import meowv64.instr.Instr

import scala.collection.mutable

trait UnitSelIO {
  val flush: Bool
  val rs: ResStationEgress
  val retire: Retirement
  val extras: mutable.HashMap[String, Data]
}

/** Read instructions from reservation stations, and send them into (probably
  * one of multiple) exec unit
  *
  * <del> All units are buffered into the same delay cycle count, so that we can
  * assert only one of them may retire an instr at within one cycle</del>
  *
  * That is not true anymore. Now we use a MPSC FIFO to consume outputs of
  * individual execution units.
  */
class UnitSel(
    gen: => Seq[ExecUnitInt],
    arbitration: Instr => Seq[Bool],
    bypassIdx: Option[Int] = None,
    /** Add additional pipeline register to improve timing
      */
    hasPipe: Boolean = true
)(implicit val coredef: CoreDef)
    extends Module
    with UnitSelIO {
  val units = gen

  val flush = IO(Input(Bool()))
  val rs = IO(Flipped(new ResStationEgress))
  val retire = IO(Output(new Retirement))

  // Extra ports
  val extras = new mutable.HashMap[String, Data]()
  for (u <- units) {
    if (u.isInstanceOf[WithCSRWriter]) {
      println("Found extra port: CSR")
      val csr = IO(new CSRWriter(coredef.XLEN))
      u.asInstanceOf[WithCSRWriter].writer <> csr
      extras.put("CSR", csr)
    }

    if (u.isInstanceOf[WithPrivPort]) {
      println("Found extra port: priv")

      val priv = extras.get("priv") match {
        case Some(port) => port
        case None => {
          val priv = IO(Input(PrivLevel()))
          extras.put("priv", priv)
          priv
        }
      }

      u.asInstanceOf[WithPrivPort].priv := priv
    }

    if (u.isInstanceOf[WithStatus]) {
      println("Found extra port: status")

      val status = extras.get("status") match {
        case Some(port) => port
        case None => {
          val status = IO(Input(new Status))
          extras.put("status", status)
          status
        }
      }

      u.asInstanceOf[WithStatus].status := status
    }
  }

  for (u <- units) {
    u.io.flush := flush
  }

  // Arbitration
  for (u <- units) {
    u.io.next := PipeInstr.empty
  }

  val execMap = Wire(Vec(units.length, Bool()))
  chisel3.dontTouch(execMap)
  execMap := arbitration(rs.instr.bits.instr.instr)

  // Contains a bypass unit, bypassing all invalid instructions to there
  // e.g. page fault, decoded exec unit might be inaccurate
  if (bypassIdx.isDefined) {
    when(rs.instr.bits.illegal) {
      execMap := VecInit(Seq.fill(units.length)(false.B))
      execMap(bypassIdx.get) := true.B
    }
  }

  // Asserts exactly one can exec this instr
  val execMapUInt = execMap.asUInt
  val execMapNoDup = !((execMapUInt -% 1.U) & execMapUInt).orR
  assert(!rs.instr.valid || execMapNoDup && execMapUInt.orR())

  val pipeExecMap = RegInit(VecInit(Seq.fill(units.length)(false.B)))
  val pipeInstr = RegInit(ReservedInstr.empty)
  val pipeInstrValid = RegInit(false.B)

  val pipeInput = Wire(Bool())

  rs.instr.ready := false.B

  if (hasPipe) {
    when(pipeInstrValid) {
      pipeInput := false.B
      for ((u, e) <- units.zip(pipeExecMap)) {
        when(e) {
          u.io.next := pipeInstr
          pipeInput := !u.io.stall
        }
      }
    }.otherwise {
      pipeInput := true.B
    }

    when(pipeInput) {
      pipeInstr := rs.instr.bits
      pipeExecMap := execMap
      pipeInstrValid := rs.instr.valid
      rs.instr.ready := rs.instr.valid
    }
  } else {
    pipeInput := DontCare
    when(rs.instr.valid) {
      rs.instr.ready := !Mux1H(execMap.zip(units.map(_.io.stall)))
      for ((u, e) <- units.zip(execMap)) {
        when(e) {
          u.io.next := rs.instr.bits
        }
      }
    }
  }

  // Retire FIFO and friends

  // Puts into retire FIFO

  val maxDepth = units.map(_.DEPTH).max
  val noDelayUnitCount = units.count(_.DEPTH == 0)
  val fifoDepth =
    units.map(_.DEPTH).sum - maxDepth + units.size - noDelayUnitCount + 2

  // One extra cell for asserting retireTail never reaches retireHead

  if (units.length == 1) {
    println("UnitSel: Single unit")
    val pipeRetire = RegInit(Retirement.empty)
    retire := pipeRetire
    when(units(0).io.stall) {
      pipeRetire := Retirement.empty
    }.otherwise {
      pipeRetire := Retirement.from(units(0).io)
    }
    when(flush) {
      pipeRetire := Retirement.empty
    }
  } else if (maxDepth == 0) {
    println("UnitSel: All units have 0 delay")
    val pipeRetire = RegInit(Retirement.empty)
    retire := pipeRetire
    val validMap = units.map(u => !u.io.stall && u.io.retired.instr.valid)
    pipeRetire := Mux1H(validMap.zip(units.map(u => Retirement.from(u.io))))
    when(!VecInit(validMap).asUInt.orR || flush) {
      pipeRetire := Retirement.empty
    }
  } else {
    println(s"UnitSel: with FIFO depth $fifoDepth")

    val retireFifo = RegInit(VecInit(Seq.fill(fifoDepth)(Retirement.empty)))
    val retireHead = RegInit(0.U(log2Ceil(fifoDepth).W))
    val retireTail = RegInit(0.U(log2Ceil(fifoDepth).W))

    var prevTail = retireTail
    for (u <- units) {
      val newTail = Wire(UInt(log2Ceil(fifoDepth).W))
      newTail := prevTail
      when(!u.io.stall && u.io.retired.instr.valid) {
        retireFifo(prevTail) := Retirement.from(u.io)
        newTail := Mux(prevTail === (fifoDepth - 1).U, 0.U, prevTail +% 1.U)
      }

      assert(prevTail === retireHead || newTail =/= retireHead)
      prevTail = newTail
    }
    retireTail := prevTail

    // Output

    when(retireTail === retireHead) {
      retire := Retirement.empty
    }.otherwise {
      retire := retireFifo(retireHead)
      retireHead := Mux(
        retireHead === (fifoDepth - 1).U,
        0.U,
        retireHead +% 1.U
      )
    }

    when(flush) {
      retireHead := 0.U
      retireTail := 0.U
    }
  }

  // Flush
  when(flush) {
    pipeInstrValid := false.B
  }
}

object UnitSel {
  class Retirement(implicit val coredef: CoreDef) extends Bundle {
    val instr = new PipeInstr
    val info = new RetireInfo
  }

  object Retirement {
    def empty(implicit coredef: CoreDef): Retirement = {
      val ret = Wire(new Retirement)
      ret.instr := PipeInstr.empty
      ret.info := RetireInfo.vacant

      ret
    }

    def from(port: ExecUnitPort)(implicit coredef: CoreDef): Retirement = {
      val ret = Wire(new Retirement)
      ret.instr := port.retired
      ret.info := port.retirement

      ret
    }
  }
}
