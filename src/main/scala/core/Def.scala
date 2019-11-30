package core
import cache._

abstract class CoreDef {
  outer =>
  val XLEN: Int = 64
  val ADDR_WIDTH: Int = 48
  val FETCH_NUM: Int = 2
  val ISSUE_NUM: Int = 2
  val ISSUE_FIFO_DEPTH: Int = FETCH_NUM * 4
  val INIT_VEC: BigInt = BigInt("FFFFFFFF8000", 16)
  val HART_ID: Int = 0

  val CYCLE_PER_TIMEUNIT: Int = 50 // We're running on 50M

  val L12_LINE_WIDTH: Int = 16 // In bytes

  object L2 extends {
    val ADDR_WIDTH: Int = outer.ADDR_WIDTH
    val ASSOC: Int = 4
    val CORE_COUNT: Int = 1
    val LINE_WIDTH: Int = outer.L12_LINE_WIDTH
    val TRANSFER_SIZE: Int = 0 // Actually ignored
    val SIZE: Int = 32768 // 32K L2
    val WB_DEPTH: Int = 4
    val XLEN: Int = outer.XLEN
  } with L2Opts

  object L1I extends {
    val ADDR_WIDTH: Int = outer.ADDR_WIDTH
    val ASSOC: Int = 4
    val LINE_WIDTH: Int = outer.L12_LINE_WIDTH
    val SIZE: Int = 4096 // 4K L1 I
    val TRANSFER_SIZE: Int = FETCH_NUM * 16
    val XLEN: Int = outer.XLEN
  } with L1Opts

  object L1D extends {
    val ADDR_WIDTH: Int = outer.ADDR_WIDTH
    val ASSOC: Int = 4
    val LINE_WIDTH: Int = outer.L12_LINE_WIDTH
    val SIZE: Int = 4096 // 4K L1 D
    val TRANSFER_SIZE: Int = outer.XLEN
    val XLEN: Int = outer.XLEN

    val WRITE_BUF_DEPTH: Int = 4
  } with L1DOpts
}

object DefaultDef extends CoreDef
