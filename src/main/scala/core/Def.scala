package core

abstract class CoreDef {
  val XLEN: Int = 64
  val ADDR_WIDTH: Int = 48
  val ISSUE_NUM: Int = 2
  val INIT_VEC: BigInt = BigInt("FFFFFFFF8000", 16)
}

object DefaultDef extends CoreDef
