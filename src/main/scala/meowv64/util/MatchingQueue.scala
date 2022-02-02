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
    val valids = RegInit(Vec(False, depth))
    val head = UInt(log2Up(depth) bits)
    val tail = UInt(log2Up(depth) bits)

    pop.valid := head =/= tail || valids(0)
    push.ready := head =/= tail || !valids(0)

    when(push.fire) {
      storage(tail) := push.payload
      valids(tail) := True
      tail := tail + 1
    }

    pop.payload := storage(head)
    when(pop.fire) {
      valids(head) := False
      head := head + 1
    }

    matched := Vec(storage.zip(valids).map({ case (t, v) => v && t.matched(matcher) })).orR
  }
}