package meowv64.multicore
import meowv64.cache.L2Opts
import meowv64.interrupt.CLINTMapping
import meowv64.interrupt.PLICDef
import meowv64.interrupt.PLICMapping

abstract class MulticoreDef(val coreCount: Int) {
  outer =>
  val CORE_COUNT = coreCount

  val INIT_VEC = BigInt(0x80000000L)

  val PADDR_WIDTH: Int = 56
  val XLEN: Int = 64

  val CYCLE_PER_TIMEUNIT: Int = 50 // We're running on 50M

  val L2_LINE_BYTES: Int = 16 // In bytes

  val INTERRUPT_CNT: Int = 15

  object L2
      extends {
        val ADDR_WIDTH: Int = outer.PADDR_WIDTH
        val ASSOC: Int = 4
        val CORE_COUNT: Int = outer.CORE_COUNT
        val LINE_BYTES: Int = outer.L2_LINE_BYTES
        val TRANSFER_WIDTH: Int = 0 // Actually ignored
        val SIZE_BYTES: Int = 16384 // 16KB L2
        val WB_DEPTH: Int = 4
        val XLEN: Int = outer.XLEN

        val MMIO = Seq(
          CLINTMapping,
          PLICMapping
        )
      }
      with L2Opts

  object PLIC
      extends {
        override val CONTEXT_COUNT: Int = outer.CORE_COUNT * 2
        override val MAX_PRIORITY: Int = 7
        override val MAX_SOURCE: Int = outer.INTERRUPT_CNT
      }
      with PLICDef
}

object DefaultDef extends MulticoreDef(coreCount = 2)
