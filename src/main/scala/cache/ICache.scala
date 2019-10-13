package cache

import chisel3._

class ICachePort(ADDR_WIDTH: Int = 48, DATA_LEN: Int = 64) extends Bundle {
  val addr = Input(UInt(ADDR_WIDTH.W))
  val read = Input(Bool())

  val stall = Output(Bool())
  val pause = Input(Bool())
  val flush = Output(Bool()) // Branch missperdict, flushing all running requests

  val data = Output(UInt(DATA_LEN.W)) // Data delay is 1 cycle
}

// TODO: Change to xpm_tdpmem

class ICache extends Bundle {
}