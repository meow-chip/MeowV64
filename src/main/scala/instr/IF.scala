package instr

import chisel3._
import _root_.cache._
import Decoder._

class IF(ADDR_WIDTH: Int = 48, FETCH_NUM: Int = 1) extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(ADDR_WIDTH.W))
    val icache = Flipped(new ICachePort(48, 32 * FETCH_NUM))
    val fetch = Input(Bool())
    val output = Output(Vec(FETCH_NUM, new Instr))

    val stall = Output(Bool())
    val pause = Input(Bool())
    val flush = Output(Bool())
  })

  io.stall <> io.icache.stall
  io.pause <> io.icache.pause
  io.flush <> io.icache.flush
  io.addr <> io.icache.addr
  io.fetch <> io.icache.read

  io.output := io.icache.data.asTypeOf(Vec(FETCH_NUM, UInt(32.W))).map(_.asInstr)
}