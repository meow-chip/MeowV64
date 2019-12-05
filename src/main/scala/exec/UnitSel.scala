package exec

import chisel3._
import _root_.core.CoreDef
import Chisel.experimental.chiselName
import chisel3.util.MuxCase
import instr.Instr
import exec.UnitSel.Retirement
import chisel3.util.log2Ceil
import _root_.core.CSRWriter
import scala.collection.mutable

/**
 * Read instructions from reservation stations, and send them into (probably one of multiple) exec unit
 * 
 * All units are buffered into the same delay cycle count, so that we can assert only one
 * of them may retire an instr at within one cycle
 */
@chiselName
class UnitSel(
  gen : => Seq[ExecUnitInt],
  arbitration: Instr => Seq[Bool]
)(implicit val coredef: CoreDef) extends MultiIOModule {
  val units = gen

  val ctrl = IO(new Bundle {
    val stall = Output(Bool())
    // You may never intentionally pause an exec unit
    val flush = Input(Bool())
  })

  val rs = IO(Flipped(new ResStationExgress))

  val retire = IO(Output(new Retirement))

  // Extra ports
  val extras = new mutable.HashMap[String, Data]()
  for(u <- units) {
    if(u.isInstanceOf[WithCSRWriter]) {
      println("Found extra port: CSR")
      val csr = IO(new CSRWriter(coredef.XLEN))
      u.asInstanceOf[WithCSRWriter].writer <> csr
      extras.put("CSR", csr)
    }
  }

  val stall = false.B
  ctrl.stall := stall

  for(u <- units) {
    u.io.flush := ctrl.flush
    u.io.pause := false.B
  }

  val maxDepth = units.map(_.DEPTH).max
  val noDelayUnitCount = units.count(_.DEPTH == 0)
  val fifoDepth = if(maxDepth == 0) {
    3
  } else {
    units.map(_.DEPTH).sum - maxDepth + units.size - noDelayUnitCount + 2
  }

  // One extra cell for asserting retireTail never reaches retireHead

  println(s"UnitSel: with FIFO depth $fifoDepth")

  val retireFifo = RegInit(VecInit(Seq.fill(fifoDepth)(Retirement.empty)))
  val retireHead = RegInit(0.U(log2Ceil(fifoDepth).W))
  val retireTail = RegInit(0.U(log2Ceil(fifoDepth).W))

  // Arbitration
  for(u <- units) {
    u.io.next := PipeInstr.empty
  }

  val execMap = arbitration(rs.instr.instr.instr)
  assume(execMap.length == units.length)
  // Asserts exactly one can exec this instr
  val execMapUInt = VecInit(execMap).asUInt
  val execMapNoDup = !((execMapUInt -% 1.U) & execMapUInt).orR
  assert(!rs.valid || execMapNoDup && execMapUInt.orR())

  rs.pop := false.B

  when(rs.valid) {
    for((u, e) <- units.zip(execMap)) {
      when(e) {
        u.io.next := rs.instr
        rs.pop := !u.io.stall
        // TODO: ignore other fields?
      }
    }
  }

  // Puts into retire FIFO 
  var prevTail = retireTail
  for(u <- units) {
    val newTail = Wire(UInt(log2Ceil(fifoDepth).W))
    newTail := prevTail
    when(!u.io.stall && !u.io.retired.instr.vacant) {
      retireFifo(prevTail) := Retirement.from(u.io)
      newTail := Mux(prevTail === (fifoDepth-1).U, 0.U, prevTail +% 1.U)
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
    retireHead := Mux(retireHead === (fifoDepth-1).U, 0.U, retireHead +% 1.U)
  }

  // Flush
  when(ctrl.flush) {
    retireHead := 0.U
    retireTail := 0.U
  }
}

object UnitSel {
  class Retirement(implicit val coredef: CoreDef)  extends Bundle {
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
