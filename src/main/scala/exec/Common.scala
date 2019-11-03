package exec

import chisel3._
import chisel3.experimental.MultiIOModule
import instr.InstrExt
import org.scalatest.tools.RerunningState

class RetireInfo(val ADDR_WIDTH: Int, val XLEN: Int) extends Bundle {
  val regWaddr = UInt(ADDR_WIDTH.W)
  val regWdata = UInt(XLEN.W)

  val branch = new BranchResult(ADDR_WIDTH)
}

class PipeInstr(val ADDR_WIDTH: Int, val XLEN: Int) extends Bundle {
  val instr = new InstrExt

  val rs1val = UInt(XLEN.W)
  val rs2val = UInt(XLEN.W)
}

class ExecUnitPort(ADDR_WIDTH: Int = 48, XLEN: Int = 64) extends Bundle {
  val next = Input(new PipeInstr(ADDR_WIDTH, XLEN))

  val stall = Output(Bool())
  val pause = Input(Bool())

  val retirement = Output(new RetireInfo(ADDR_WIDTH, XLEN))
  val retired = Output(new PipeInstr(ADDR_WIDTH, XLEN))
}

abstract class ExecUnit[T <: Data](
  DEPTH: Int,
  ExtData: T,
  ADDR_WIDTH: Int = 48,
  XLEN: Int = 64
) extends MultiIOModule {
  val io = IO(new ExecUnitPort(ADDR_WIDTH, XLEN))

  def init(): Unit = {
    if(DEPTH != 0) {
      val storeInit = Wire(Vec(DEPTH, new Bundle {
        val pipe = new PipeInstr(ADDR_WIDTH, XLEN)
        val ext = ExtData.cloneType
      }))

      for(i <- (0 until DEPTH)) {
        storeInit(i) := DontCare
        storeInit(i).pipe.instr.instr.imm := 0.S // Treadle bug?
        storeInit(i).pipe.instr.vacant := true.B
        storeInit(i).ext := DontCare
      }
      val current = RegInit(storeInit)

      // Default push
      val (fExt, fStall) = connectStage(0, io.next, None)
      var stall = fStall

      when(!io.stall && !io.pause) {
        current(0).pipe := io.next
        current(0).ext := fExt
      }

      for(i <- (1 until DEPTH)) {
        val (nExt, sStall) = connectStage(i, current(i-1).pipe, Some(current(i-1).ext))
        when(!io.stall && !io.pause) {
          current(i).pipe := current(i-1).pipe
          current(i).ext := nExt
        }
        stall = stall || sStall
      }

      val (nExt, lStall) = connectStage(DEPTH, current(DEPTH-1).pipe, Some(current(DEPTH-1).ext))
      io.retired := current(DEPTH-1).pipe
      when(io.retired.instr.vacant) {
        io.retirement := vacantFinalize()
      }.otherwise {
        io.retirement := finalize(current(DEPTH-1).pipe, nExt)
      }
      io.stall := stall || lStall
    } else {
      val (nExt, sStall) = connectStage(0, io.next, None)
      // Use chisel's unconnected wire check to enforce that no ext is exported from this exec unit
      io.retired := io.next
      when(io.retired.instr.vacant) {
        io.retirement := vacantFinalize()
      }.otherwise {
        io.retirement := finalize(io.next, nExt)
      }
      io.stall := sStall
    }

    // Override reg write during stall
    // TODO: remove after switching to tumasulo
    when(io.stall) {
      io.retirement.regWaddr := 0.U
    }
  }

  def map(stage: Int, pipe: PipeInstr, ext: Option[T]): (T, Bool)

  def finalize(pipe: PipeInstr, ext: T): RetireInfo 

  def vacantFinalize(): RetireInfo = {
    val info = Wire(new RetireInfo(ADDR_WIDTH, XLEN))

    info.branch.nofire()
    info.regWaddr := 0.U
    info.regWdata := DontCare

    info
  }

  def connectStage(stage: Int, pipe: PipeInstr, ext: Option[T]): (T, Bool) = {
    val nExt = Wire(ExtData.cloneType)
    val sStall = Wire(Bool())

    when(pipe.instr.vacant) {
      nExt := DontCare
      sStall := false.B
    }.otherwise {
      val (ce, stall) = map(stage, pipe, ext)
      nExt := ce
      sStall := stall
    }

    (nExt, sStall)
  }
}
