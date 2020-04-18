package paging

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.util.RRArbiter
import _root_.core.CoreDef
import _root_.core.Satp
import _root_.util.FlushableSlot
import cache.DCReader
import _root_.core.SatpMode

object PTWState extends ChiselEnum {
  val idle, reading, read = Value
}

class PTW(implicit coredef: CoreDef) extends MultiIOModule {
  val itlb = IO(Flipped(new TLBExt))
  val dtlb = IO(Flipped(new TLBExt))

  val satp = IO(Input(new Satp))
  val dc = IO(new DCReader)
  dc.req.noenq()

  val arbiter = Module(new RRArbiter(UInt(coredef.vpnWidth.W), 2))
  arbiter.io.in(0) <> itlb.req
  arbiter.io.in(1) <> dtlb.req
  arbiter.io.out.nodeq()

  val MAX_SEG = coredef.vpnWidth / 9

  val state = RegInit(PTWState.idle)
  val level = RegInit(0.U(log2Ceil(MAX_SEG).W))

  val segs = VecInit((0 until MAX_SEG).map({ case idx => {
    arbiter.io.out.bits((MAX_SEG-idx)*9 - 1, (MAX_SEG - idx-1)*9)
  }}))
  val seg = segs(level)

  val resp = Wire(new PTE)
  val fault = WireDefault(false.B)
  resp := DontCare
  itlb.resp := resp
  dtlb.resp := resp
  itlb.fault := fault
  dtlb.fault := fault

  itlb.level := level
  dtlb.level := level

  val dcSlot = Module(new FlushableSlot(UInt(), false, true))
  dcSlot.io.flush := false.B
  dcSlot.io.enq.bits := dc.resp.bits
  dcSlot.io.enq.valid := dc.resp.valid
  when(dc.resp.valid) {
    assert(dcSlot.io.enq.ready)
  }
  dcSlot.io.deq.nodeq()

  switch(state) {
    is(PTWState.idle) {
      assert(!dcSlot.io.deq.valid)
      when(arbiter.io.out.valid) {
        level := 0.U

        val initSeg = Wire(UInt())
        when(satp.mode === SatpMode.sv39) {
          level := 1.U // Skip level 0 in sv39
          initSeg := segs(1)
        }.otherwise {
          level := 0.U
          initSeg := segs(0)
        }

        dc.req.enq(satp.ppn ## initSeg ## 0.U(3.W)) // PTE are aligned in 64-bits
        when(dc.req.fire()) {
          state := PTWState.reading
        }
      }
    }

    is(PTWState.reading) {
      // Data is in dcHold
      val pte = dcSlot.io.deq.bits.asTypeOf(new PTE)

      when(!dcSlot.io.deq.valid) {
        // Waiting for DC reply
      }.elsewhen(pte.valid) {
        when(pte.intermediate) {
          when(level === (MAX_SEG-1).U) {
            // Reached last level
            state := PTWState.idle
            resp := pte
            fault := true.B
            arbiter.io.out.deq()
            dcSlot.io.deq.deq()
          } otherwise {
            // Continue searching
            dc.req.enq(pte.ppn ## segs(level + 1.U) ## 0.U(3.W))
            level := level + 1.U
            when(dc.req.fire()) { // Wait for request to go in
              dcSlot.io.deq.deq()
            }
          }
        } otherwise {
          // Validate superpage alignment
          fault := pte.misaligned(level)
          state := PTWState.idle
          resp := pte
          arbiter.io.out.deq()
          dcSlot.io.deq.deq()
        }
      } otherwise {
        state := PTWState.idle
        resp := PTE.empty
        fault := true.B
        arbiter.io.out.deq()
        dcSlot.io.deq.deq()
      }
    }
  }
}
