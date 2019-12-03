package exec

import chisel3._
import _root_.core.CoreDef
import Chisel.experimental.chiselName
import chisel3.util.MuxCase
import instr.Instr
import exec.UnitSel.Retirement

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

  def buffer(input: Retirement, length: Int, pause: Bool, flush: Bool): Retirement = {
    val init = Wire(new Retirement)
    init.info := DontCare
    init.instr := PipeInstr.empty

    (0 until length).foldLeft(input)((cur, idx) => {
      val next = RegInit(init)
      when(flush) {
        next := init
      }.elsewhen(!pause) {
        next := cur
      }
      next
    })
  }

  val ctrl = IO(new Bundle {
    val stall = Output(Bool())
    // You may never intentionally pause an exec unit
    val flush = Input(Bool())
  })

  val rs = IO(Flipped(new ResStationExgress))

  val retire = IO(Output(new Retirement))

  val stall = units.foldLeft(false.B)((acc, u) => acc || u.io.stall)
  ctrl.stall := stall

  for(u <- units) {
    u.io.flush := ctrl.flush
    u.io.pause := stall
  }

  val maxDepth = units.map(_.DEPTH).max
  val buffered = units.map(u => {
    val r = Wire(new Retirement)
    r.info := u.io.retirement
    r.instr := u.io.retired

    buffer(r, maxDepth - u.DEPTH, stall, ctrl.flush)
  })

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

  val fire = !stall && rs.valid
  rs.pop := fire

  when(rs.valid) {
    for((u, e) <- units.zip(execMap)) {
      when(e) {
        u.io.next := rs.instr
        // TODO: ignore other fields?
      }
    }
  }

  // Output

  // Asserts that only one can retire instructions within a cycle
  val retireMask = VecInit(buffered.map(u => !u.instr.instr.vacant)).asUInt()
  val retireNoDup = !((retireMask -% 1.U) & retireMask).orR() // At most one retirement
  assert(stall || retireNoDup) // !stall -> retireSingle

  retire := MuxCase(Retirement.empty, buffered.map(r => (!r.instr.instr.vacant, r)))
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
  }
}
