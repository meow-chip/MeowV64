package meowv64.util

import chisel3._
import chisel3.util._

object FirstOneOH {
  def apply(bits: UInt, prio: UInt): UInt = {
    val p = if(prio == null) 1.U(bits.getWidth.W) else prio
    val double_bits = bits ## (bits & ~(0.U(1.W) ## RightOr(p) >> 1))
    val double_grant = double_bits & ~(LeftOr(double_bits) << 1)(double_bits.getWidth - 1, 0)
    (double_grant >> bits.getWidth) | double_grant(bits.getWidth - 1, 0)
  }

  def apply(bits: UInt): UInt = {
    apply(bits, null)
  }

  def apply(bits: Seq[Bool], prio: UInt = null): UInt = {
    apply(VecInit(bits).asUInt, prio)
  }
}

object Reverse {
  def apply(u: UInt): UInt = VecInit(u.asBools.reverse).asUInt
}

object LeftOr {
  def apply(data: UInt): UInt = VecInit(Seq.tabulate(data.getWidth) { i: Int =>
    VecInit(data.asBools().dropRight(data.getWidth - i - 1)).asUInt().orR()
  }).asUInt()
}

object RightOr {
  def apply(data: UInt): UInt = VecInit(Seq.tabulate(data.getWidth) { i: Int =>
    VecInit(data.asBools().drop(i)).asUInt().orR()
  }).asUInt()
}