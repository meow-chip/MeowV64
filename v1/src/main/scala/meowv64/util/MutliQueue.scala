package meowv64.util
import chisel3._
import chisel3.util._

class MultiQueueIO[T <: Data](private val gen: T, val THROUGHPUT: Int)
    extends Bundle {
  val view = Input(Vec(THROUGHPUT, gen))
  val cnt = Input(UInt(log2Ceil(THROUGHPUT + 1).W))
  val accept = Output(UInt(log2Ceil(THROUGHPUT + 1).W))
}

class MultiQueue[T <: Data](
    private val gen: T,
    val DEPTH: Int,
    val INPUT: Int,
    val OUTPUT: Int
) extends Module {
  val reader = IO(Flipped(new MultiQueueIO(gen, OUTPUT)))
  val writer = IO(new MultiQueueIO(gen, INPUT))
  val flush = IO(Input(Bool()))

  // val CNT = Integer.max(coredef.FETCH_NUM, coredef.ISSUE_NUM)
  // val SIZE = (coredef.ISSUE_FIFO_DEPTH.toDouble / CNT).ceil.toInt
  val CNT = Integer.max(INPUT, OUTPUT)

  val queues = (0 until CNT).map(idx => {
    val mod = Module(new FlushableQueue(gen, DEPTH))
    mod.suggestName(s"queue_$idx")
    mod
  })

  for (q <- queues) {
    q.io.flush.get := flush
  }

  val readies = PopCount(queues.map(_.io.enq.ready))
  val valids = PopCount(queues.map(_.io.deq.valid))
  reader.cnt := valids.min(OUTPUT.U)
  writer.accept := readies.min(INPUT.U)

  val enqs = VecInit(queues.map(_.io.enq))
  val deqs = VecInit(queues.map(_.io.deq))

  for (enq <- enqs) enq.noenq()
  for (deq <- deqs) deq.nodeq()

  val wptr = RegInit(0.U(log2Ceil(CNT).W))
  val rptr = RegInit(0.U(log2Ceil(CNT).W))

  for (i <- (0 until INPUT)) {
    when(writer.cnt > i.U) {
      enqs(wptr + i.U).enq(writer.view(i))
      assert(enqs(wptr + i.U).fire)
    }
  }

  for (i <- (0 until OUTPUT)) {
    when(deqs(rptr + i.U).valid) {
      reader.view(i) := deqs(rptr + i.U).bits
    } otherwise {
      // set to zero when invalid
      reader.view(i) := 0.U.asTypeOf(gen)
    }

    when(reader.accept > i.U) {
      deqs(rptr + i.U).deq()
      assert(deqs(rptr + i.U).fire)
    }
  }

  rptr := rptr +% reader.accept
  wptr := wptr +% writer.cnt

  when(flush) {
    wptr := 0.U
    rptr := 0.U
  }
}
