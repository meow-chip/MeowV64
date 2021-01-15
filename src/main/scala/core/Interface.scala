package core

import chisel3._
import chisel3.util._
import chisel3.experimental._

object MemOp extends ChiselEnum {
  val ReadMiss = Value
  val ReadGrant = Value

  val WriteMiss = Value
  val WriteGrant = Value
  ???
}

class MemPacket(val ADDR_BIT: Int, val DATA_BYTE: Int) extends Bundle {
  val op = MemOp()
  val addr = UInt(ADDR_BIT.W)
  val data = Vec(DATA_BYTE, UInt(8.W))
  ???
}

class MemBus(val ADDR_BIT: Int, val DATA_BYTE: Int) extends Bundle {
  val out = DecoupledIO(new MemPacket(ADDR_BIT, DATA_BYTE))
  ???
}
