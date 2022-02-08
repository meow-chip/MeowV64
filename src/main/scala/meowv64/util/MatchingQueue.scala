package meowv64.util

import chisel3._
import chisel3.util._

trait MatchedData[T <: Data] extends Data {
  def matched(matcher: T): Bool
}

// Non fallthrough only!
class MatchingQueue[M <: Data, T <: MatchedData[M]](t: T, m: M, depth: Int) extends Module {
  val push = IO(Flipped(DecoupledIO(t)))
  val pop = IO(DecoupledIO(t))
  val matcher = IO(Input(m))
  val matched = if(depth == 0) null else IO(Output(UInt(depth.W)))

  require(isPow2(depth))

  if(depth == 0) {
    pop.valid := push.valid
    push.ready := pop.ready
    pop.bits := push.bits
  } else {
    val storage = Reg(Vec(depth, t))
    val valids = RegInit(0.U(depth.W))
    val ptr = RegInit(0.U(log2Ceil(depth).W))

    val nptr = Wire(ptr.cloneType)
    nptr := ptr

    pop.bits := storage(0)
    pop.valid := valids(0)

    push.ready := !valids(depth-1)

    when(push.fire) {
      val pptr = Mux(pop.fire, ptr - 1.U, ptr)
      storage(pptr) := push.bits
    }

    when(push.fire =/= pop.fire) {
      ptr := Mux(push.fire, ptr + 1.U, ptr - 1.U)
      valids := Mux(push.fire, valids << 1 | 1.U, valids >> 1)
    }

    matched := VecInit(storage.zip(valids.asBools).map({ case (t, v) => v && t.matched(matcher) })).asUInt
  }
}