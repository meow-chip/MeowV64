package meowv64.util

import chisel3._
import chisel3.experimental._
import chisel3.util._

class FlushableQueueIO[T <: Data](private val gen: T, val _entries: Int)
    extends QueueIO(gen, _entries) {
  val flush = Input(Bool())
}

/** Copied from chisel3.util.Queue
  */
@chiselName
class FlushableQueue[T <: Data](
    gen: T,
    val entries: Int,
    pipe: Boolean = false,
    flow: Boolean = false
)(implicit compileOptions: chisel3.CompileOptions)
    extends Module() {
  require(entries > -1, "Queue must have non-negative number of entries")
  require(entries != 0, "Use companion object Queue.apply for zero entries")
  val genType = if (compileOptions.declaredTypeMustBeUnbound) {
    requireIsChiselType(gen)
    gen
  } else {
    if (DataMirror.internal.isSynthesizable(gen)) {
      chiselTypeOf(gen)
    } else {
      gen
    }
  }

  val io = IO(new FlushableQueueIO(genType, entries))

  private val ram = SyncReadMem(entries, genType)
  private val enq_ptr = Counter(entries)
  private val deq_ptr = new PreviewCounter(entries)
  private val maybe_full = RegInit(false.B)

  private val ptr_match = enq_ptr.value === deq_ptr.value
  private val empty = ptr_match && !maybe_full
  private val full = ptr_match && maybe_full
  private val do_enq = WireDefault(io.enq.fire())
  private val do_deq = WireDefault(io.deq.fire())

  when(do_enq) {
    ram(enq_ptr.value) := io.enq.bits
    enq_ptr.inc()
  }
  when(do_deq) {
    deq_ptr.inc()
  }
  when(do_enq =/= do_deq) {
    maybe_full := do_enq
  }

  val lastCycleRAW = RegNext(deq_ptr.preview === enq_ptr.value && do_enq)
  val lastCycleData = RegNext(io.enq.bits)

  io.deq.valid := !empty
  io.enq.ready := !full
  io.deq.bits := Mux(lastCycleRAW, lastCycleData, ram(deq_ptr.preview))

  if (flow) {
    when(io.enq.valid) { io.deq.valid := true.B }
    when(empty) {
      io.deq.bits := io.enq.bits
      do_deq := false.B
      when(io.deq.ready) { do_enq := false.B }
    }
  }

  if (pipe) {
    when(io.deq.ready) { io.enq.ready := true.B }
  }

  private val ptr_diff = enq_ptr.value - deq_ptr.value
  if (isPow2(entries)) {
    io.count := Mux(maybe_full && ptr_match, entries.U, 0.U) | ptr_diff
  } else {
    io.count := Mux(
      ptr_match,
      Mux(maybe_full, entries.asUInt, 0.U),
      Mux(deq_ptr.value > enq_ptr.value, entries.asUInt + ptr_diff, ptr_diff)
    )
  }

  when(io.flush) {
    if (entries != 1) {
      enq_ptr.value := 0.U
      deq_ptr.value := 0.U
    }
    maybe_full := false.B
  }
}

/** Copied from chisel3.util.Queue
  */
@chiselName
class FlushableSlot[T <: Data](
    gen: T,
    pipe: Boolean = false,
    flow: Boolean = false
)(implicit compileOptions: chisel3.CompileOptions)
    extends Module() {
  val genType = if (compileOptions.declaredTypeMustBeUnbound) {
    requireIsChiselType(gen)
    gen
  } else {
    if (DataMirror.internal.isSynthesizable(gen)) {
      chiselTypeOf(gen)
    } else {
      gen
    }
  }

  val io = IO(new FlushableQueueIO(genType, 1))

  private val slot = Reg(genType)
  private val occupied = RegInit(false.B)
  private val do_enq = WireDefault(io.enq.fire())
  private val do_deq = WireDefault(io.deq.fire())

  when(do_enq) {
    slot := io.enq.bits
  }

  when(do_enq =/= do_deq) {
    occupied := do_enq
  }

  io.deq.valid := occupied
  io.enq.ready := !occupied
  io.deq.bits := slot

  if (flow) {
    when(io.enq.valid) { io.deq.valid := true.B }
    when(!occupied) {
      io.deq.bits := io.enq.bits
      do_deq := false.B
      when(io.deq.ready) { do_enq := false.B }
    }
  }

  if (pipe) {
    when(io.deq.ready) { io.enq.ready := true.B }
  }

  io.count := Mux(occupied, 1.U, 0.U)

  when(io.flush) {
    occupied := false.B
  }
}
