package core.cache

import chisel3._
import _root_.core._

class ICache(implicit val config: Config) extends MultiIOModule {
  val addr = IO(Input(new PAddr))
  val flush = IO(Input(Bool()))

  val fetched = IO(Output(Vec(
    config.fetchWidth,
    UInt(16.W)
  )))

  ???
}