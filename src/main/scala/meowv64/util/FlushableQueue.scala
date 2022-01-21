package meowv64.util

import chisel3._
import chisel3.util._

class FlushableQueueIO[T <: Data](private val gen: T, val _entries: Int)
    extends QueueIO(gen, _entries, hasFlush = true) {}

/** Flushable chisel3.util.Queue
  */
class FlushableQueue[T <: Data](
    gen: T,
    entries: Int,
    pipe: Boolean = false,
    flow: Boolean = false
) extends Queue(gen, entries, pipe, flow, hasFlush = true) {}

/** Flushable one entry chisel3.util.Queue
  */
class FlushableSlot[T <: Data](
    gen: T,
    pipe: Boolean = false,
    flow: Boolean = false
) extends Queue(gen, 1, pipe, flow, hasFlush = true) {}
