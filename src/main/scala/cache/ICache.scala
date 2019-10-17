package cache

import chisel3._
import _root_.data._

class ICachePort(ADDR_WIDTH: Int, DATA_LEN: Int) extends Bundle {
  val addr = Input(UInt(ADDR_WIDTH.W))
  val read = Input(Bool())

  val axi = new AXI(8)

  val stall = Output(Bool())
  val pause = Input(Bool())
  val flush = Output(Bool()) // Branch missperdict, flushing all running requests

  val data = Output(UInt(DATA_LEN.W)) // Data delay is 1 cycle
}

// TODO: Change to xpm_tdpmem
class ICache(ADDR_WIDTH: Int, DATA_LEN: Int) extends Module {
  val io = IO(new ICachePort(ADDR_WIDTH, DATA_LEN))

  val inner = Module(new Passthrough(ADDR_WIDTH, DATA_LEN))

  inner.io.addr <> io.addr
  inner.io.read <> io.read
  inner.io.write := false.B
  inner.io.axi := io.axi
  inner.io.stall := io.stall
  inner.io.pause := io.pause
  inner.io.rdata := io.data

  // FIXME: make passthrough supports flush
  io.flush <> DontCare
}