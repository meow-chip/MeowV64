package meowv64.config

import chisel3._

object Consts {
  val INSTR_WIDTH: Int = 16
  val MAX_PADDR_WIDTH: Int = 57

  val ibits: UInt = UInt(INSTR_WIDTH.W)
}