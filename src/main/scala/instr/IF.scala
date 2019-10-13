package instr

import chisel3._
import _root_.cache._

class IF(ADDR_WIDTH: Int = 48, ISSUE_NUM: Int = 1) extends Module {
  val io = IO(new Bundle {
    val icache = Flipped(new ICachePort(48, 32 * ISSUE_NUM))
    val output = Output(new Instr())

    val stall = Output(Bool())
    val pause = Input(Bool())
    val flush = Output(Bool())
  })
}