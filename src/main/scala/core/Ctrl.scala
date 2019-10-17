package core

import chisel3._

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
    val branch = Input(Bool())
    val baddr = Input(UInt(ADDR_WIDTH.W))

    val fetch = StageCtrl.ctrl()
    val exec = StageCtrl.ctrl()
  })

  val pc = RegInit(INIT_VEC.U(ADDR_WIDTH.W))

  val stalled = io.fetch.stall || io.exec.stall
  io.fetch.pause := stalled
  io.exec.pause := stalled

  io.fetch.flush := false.B
  io.exec.flush := false.B

  when(!stalled) {
    when(io.branch) {
      io.fetch.flush := true.B
      pc := io.baddr
    }.otherwise {
      pc := pc + (4 * ISSUE_NUM).U
    }
  }
}