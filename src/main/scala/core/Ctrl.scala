package core

import chisel3._
import chisel3.util._

class StageCtrl extends Bundle {
  val stall = Input(Bool())
  val flush = Output(Bool())
}

object StageCtrl {
  def ctrl() = new StageCtrl
  def stage() = Flipped(new StageCtrl)
}

class Ctrl(ADDR_WIDTH: Int, INIT_VEC: BigInt, FETCH_NUM: Int) extends Module {
  val io = IO(new Bundle{
    val pc = Output(UInt(ADDR_WIDTH.W))
    val skip = Output(UInt(log2Ceil(FETCH_NUM).W))

    val branch = Input(Bool())
    val baddr = Input(UInt(ADDR_WIDTH.W))

    val fetch = StageCtrl.ctrl()
    val exec = StageCtrl.ctrl()
  })

  val pc = RegInit(INIT_VEC.U(ADDR_WIDTH.W))
  io.pc := pc

  io.fetch.flush := false.B
  io.exec.flush := false.B

  io.skip := 0.U

  // Rst comes together with an branch
  // TODO: impl rst (a.k.a. FENCE.I)
  val performingBranch = io.branch && !io.exec.stall

  // IF control && PC controllert
  when(performingBranch) {
    // printf(p"Branched, baddr: ${Hexadecimal(io.baddr)}\n")
    io.fetch.flush := true.B
    assert(!io.fetch.stall)

    val instrOffset = log2Ceil(Const.INSTR_MIN_WIDTH / 8)
    val issueOffset = log2Ceil(FETCH_NUM)
    val pcAlign = instrOffset + issueOffset
    val alignedPC = io.baddr(ADDR_WIDTH-1, pcAlign) ## 0.U(pcAlign.W)

    pc := alignedPC + (Const.INSTR_MIN_WIDTH / 8 * FETCH_NUM).U
    io.pc := alignedPC
    io.skip := io.baddr(pcAlign, 0) >> instrOffset
  }.elsewhen(!io.fetch.stall) {
    // printf(p"PC: ${Hexadecimal(io.pc)}\n")
    pc := pc + (Const.INSTR_MIN_WIDTH / 8 * FETCH_NUM).U
  }

  // Exec ctrl
  io.exec.flush := performingBranch

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

  // Avoid Vivado naming collision. Com'on, Xilinx, write *CORRECT* code plz
  override def desiredName: String = "PipeCtrl"
}
