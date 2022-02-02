package meowv64.util

import spinal.core._
import spinal.lib._

trait MatchedData[T <: Data] extends Data {
  def matched(matcher: T): Bool
}

// Non fallthrough only!
class MatchingQueue[M <: Data, T <: MatchedData[M]](t: T, m: M, depth: Int) extends Component {
  val push = slave Stream(t)
  val pop = master Stream(t)
  val matcher = in(m)
  val matched = out Bool()

  require(isPow2(depth))

  if(depth == 0) {
    pop.valid := push.valid
    push.ready := pop.ready
    pop.payload := push.payload
    matched := False
  } else {
    val storage = Reg(Vec(t, depth))
    val valids = RegInit(B(0, depth bits))
    val ptr = RegInit(U(0, log2Up(depth) bits))

    val nptr = ptr.clone();
    nptr := ptr

    pop.payload := storage(0)
    pop.valid := valids(0)

    push.ready := !valids(depth-1)

    when(push.fire) {
      val pptr = Mux(pop.fire, ptr - 1, ptr)
      storage(pptr) := push.payload
    }

    when(push.fire =/= pop.fire) {
      ptr := Mux(push.fire, ptr + 1, ptr - 1)
      valids := Mux(push.fire, valids << 1 | 1, valids >> 1)
    }

    matched := Vec(storage.zip(valids.asBools).map({ case (t, v) => v && t.matched(matcher) })).orR
  }
}