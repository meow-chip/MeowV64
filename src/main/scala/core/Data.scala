package core

import chisel3._

class PAddr(implicit val config: Config) extends Bundle {
  val offset = UInt(12.W)
  val ppn = UInt((config.pAddrWidth - 12).W)
}