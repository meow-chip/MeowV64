package core

import chisel3._
import chisel3.util._

class StageCtrl extends Bundle {
  val stall = Input(Bool())
  val pause = Output(Bool())
  val flush = Output(Bool())
}

object StageCtrl {
  def ctrl() = new StageCtrl
  def stage() = Flipped(new StageCtrl)
}

class Ctrl(ADDR_WIDTH: Int, INIT_VEC: BigInt, ISSUE_NUM: Int) extends Module {
  val io = IO(new Bundle{
    val pc = Output(UInt(ADDR_WIDTH.W))
    val skip = Output(UInt(log2Ceil(ISSUE_NUM).W))

    val branch = Input(Bool())
    val baddr = Input(UInt(ADDR_WIDTH.W))

    val fetch = StageCtrl.ctrl()
    val exec = StageCtrl.ctrl()
  })

  val pc = RegInit(INIT_VEC.U(ADDR_WIDTH.W))
  io.pc := pc

  val stalled = io.fetch.stall || io.exec.stall
  io.fetch.pause := stalled
  io.exec.pause := stalled

  io.fetch.flush := false.B
  io.exec.flush := false.B

  io.skip := 0.U

  when(!stalled) {
    when(io.branch) {
      // printf(p"Branched, baddr: ${Hexadecimal(io.baddr)}\n")
      io.fetch.flush := true.B
      io.exec.flush := true.B

      val instrOffset = log2Ceil(Const.INSTR_MIN_WIDTH / 8)
      val issueOffset = log2Ceil(ISSUE_NUM)
      val pcAlign = instrOffset + issueOffset
      val alignedPC = io.baddr(ADDR_WIDTH-1, pcAlign) ## 0.U(pcAlign.W)

      pc := alignedPC + (Const.INSTR_MIN_WIDTH / 8 * ISSUE_NUM).U
      io.pc := alignedPC
      io.skip := io.baddr(pcAlign, 0) >> instrOffset
    }.otherwise {
      pc := pc + (Const.INSTR_MIN_WIDTH / 8 * ISSUE_NUM).U
    }
    // printf(p"PC: ${Hexadecimal(io.pc)}\n")
  }

  /*
  printf("Ctrl status:\n")
  printf("================\n")
  printf(p"PC: ${pc}\n")
  printf(p"Stalled: ${stalled}\n")
  when(io.fetch.stall) {
    printf("  Fetch stall")
  }
  when(io.exec.stall) {
    printf("  Exec stall")
  }
  printf("\n")
  */
}
