package cache

import chisel3._
import _root_.data._
import chisel3.util.log2Ceil

class I$Port(val ADDR_WIDTH: Int, val DATA_LEN: Int, val XLEN: Int) extends Bundle {
  val addr = Input(UInt(ADDR_WIDTH.W))
  val read = Input(Bool())

  val stall = Output(Bool())
  val pause = Input(Bool())
  val flush = Input(Bool()) // Branch missperdict, flushing all running requests

  val data = Output(UInt(DATA_LEN.W)) // Data delay is 1 cycle
  val vacant = Output(Bool())
}

// TODO: Change to xpm_tdpmem
abstract class L1I$(ADDR_WIDTH: Int, DATA_LEN: Int, XLEN: Int) extends Module {
}
