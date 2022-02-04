package meowv64.util

import spinal.core._

object ExpandInterleave extends Function2[Bits, Int, Bits] {
  override def apply(data: Bits, ratio: Int): Bits = {
    Vec(data.asBools.map(b => Vec((0 to ratio).map(_ => b)))).as(Bits(data.getWidth * ratio bits))
  }
}

object Duplicate extends Function2[Bits, Int, Bits] {
  override def apply(data: Bits, times: Int): Bits = {
    Vec((0 to times).map(_ => data)).as(Bits(data.getWidth * times bits))
  }
}