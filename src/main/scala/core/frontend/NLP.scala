package core.frontend

import _root_.core.Config
import chisel3._
import chisel3.util._

/**
 * Next line predictor
 */

class NLP(implicit val config: Config) extends MultiIOModule {
  val pc = IO(Input(UInt(config.xlen.W)))
  val npc = IO(Output(UInt(config.xlen.W)))

  val pcAligned = pc(config.xlen-1, config.fetchOffset) ## 0.U(config.fetchOffset.W)
  npc := pcAligned + (config.fetchWidth * 2).U
}