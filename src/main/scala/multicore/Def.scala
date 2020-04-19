package multicore

import core.CoreDef
import cache.L2Opts

abstract class MulticoreDef {
  outer =>
  val CORE_COUNT = 1

  val INIT_VEC = BigInt(0x80000000L)

  val PADDR_WIDTH: Int = 56
  val XLEN: Int = 64

  val CYCLE_PER_TIMEUNIT: Int = 50 // We're running on 50M

  val L2_LINE_WIDTH: Int = 16 // In bytes

  object L2 extends {
    val ADDR_WIDTH: Int = outer.PADDR_WIDTH
    val ASSOC: Int = 4
    val CORE_COUNT: Int = outer.CORE_COUNT
    val LINE_WIDTH: Int = outer.L2_LINE_WIDTH
    val TRANSFER_SIZE: Int = 0 // Actually ignored
    val SIZE: Int = 16384 // 16K L2
    val WB_DEPTH: Int = 4
    val XLEN: Int = outer.XLEN
  } with L2Opts
}

object DefaultDef extends MulticoreDef
