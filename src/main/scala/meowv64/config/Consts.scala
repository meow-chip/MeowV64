package meowv64.config

import spinal.core._

object Consts {
  val INSTR_WIDTH: Int = 16
  val MAX_PADDR_WIDTH: Int = 57

  val ibits: Bits = Bits (INSTR_WIDTH bits)
}