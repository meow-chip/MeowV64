package core
import cache._

abstract class CoreDef {
  outer =>
  val XLEN: Int = 64
  val ADDR_WIDTH: Int = 48
  val FETCH_NUM: Int = 2
  val ISSUE_NUM: Int = 2
  val RETIRE_NUM: Int = 2
  val ISSUE_FIFO_DEPTH: Int = FETCH_NUM * 4
  val INIT_VEC: BigInt = BigInt("FFFF20000000", 16)

  val BHT_SIZE: Int = 64
  val BHT_WIDTH: Int = 2

  val HART_ID: Int = 0

/**
 * This is one larger than the actual maximum number,
 * because we are reserving name 0 for reg 0
 */
  val INFLIGHT_INSTR_LIMIT = 8 
  val UNIT_COUNT: Int = 3
  val RESERVATION_STATION_DEPTHS = Seq(
    4,
    2,
    2
  )

  val CYCLE_PER_TIMEUNIT: Int = 50 // We're running on 50M

  val L12_LINE_WIDTH: Int = 16 // In bytes

  object L2 extends {
    val ADDR_WIDTH: Int = outer.ADDR_WIDTH
    val ASSOC: Int = 4
    val CORE_COUNT: Int = 1
    val LINE_WIDTH: Int = outer.L12_LINE_WIDTH
    val TRANSFER_SIZE: Int = 0 // Actually ignored
    val SIZE: Int = 16384 // 32K L2
    val WB_DEPTH: Int = 4
    val XLEN: Int = outer.XLEN
  } with L2Opts

  object L1I extends {
    val ADDR_WIDTH: Int = outer.ADDR_WIDTH
    val ASSOC: Int = 2
    val LINE_WIDTH: Int = outer.L12_LINE_WIDTH
    val SIZE: Int = 2048 // 4K L1 I
    val TRANSFER_SIZE: Int = FETCH_NUM * 16
    val XLEN: Int = outer.XLEN
  } with L1Opts

  object L1D extends {
    val ADDR_WIDTH: Int = outer.ADDR_WIDTH
    val ASSOC: Int = 2
    val LINE_WIDTH: Int = outer.L12_LINE_WIDTH
    val SIZE: Int = 2048 // 4K L1 D
    val TRANSFER_SIZE: Int = outer.XLEN
    val XLEN: Int = outer.XLEN

    val WRITE_BUF_DEPTH: Int = 4
  } with L1DOpts
}

object DefaultDef extends CoreDef
