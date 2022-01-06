package meowv64.util

import chisel3._
import chisel3.util._
import chisel3.experimental._

@chiselName
class PreviewCounter(val n: Int) {
  require(n >= 0)
  val value = if (n > 1) RegInit(0.U(log2Ceil(n).W)) else 0.U
  val preview = WireDefault(value)
  if(n > 1) value := preview

  def inc(): Bool = {
    if (n > 1) {
      val wrap = value === (n-1).asUInt
      preview := value + 1.U
      if (!isPow2(n)) {
        when (wrap) { preview := 0.U }
      }
      wrap
    } else {
      true.B
    }
  }
}
