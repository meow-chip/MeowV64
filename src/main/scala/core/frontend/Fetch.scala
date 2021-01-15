package core.frontend

import chisel3._
import chisel3.util._
import _root_.core.Config

// TODO: figuring out the delay
class Fetch(implicit val config: Config) extends MultiIOModule {
  val br = IO(Flipped(Valid(UInt(config.xlen.W))))
  val fetched = IO(Decoupled(new FetchPacket))
}