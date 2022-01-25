package meowv64.interrupt

import chisel3._
import chisel3.experimental._
import chisel3.util._

abstract class MMIODef {
  val ADDR_WIDTH: Int
  val XLEN: Int
}

abstract class MMIOMapping {
  val MAPPED_START: BigInt
  val MAPPED_SIZE: BigInt
}

object MMIOReqOp extends ChiselEnum {
  val read, write = Value
}

class MMIOReq(val mmiodef: MMIODef) extends Bundle {
  val addr = UInt(mmiodef.ADDR_WIDTH.W)
  val wdata = UInt(mmiodef.XLEN.W)
  val op = MMIOReqOp()
}

/** MMIO access port from L2 to CLINT/PLIC
  */
class MMIOAccess(val mmiodef: MMIODef) extends Bundle {
  val req = Flipped(Decoupled(new MMIOReq(mmiodef)))
  val resp = Valid(UInt(mmiodef.XLEN.W))
}
