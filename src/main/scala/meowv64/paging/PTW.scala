package meowv64.paging

import chisel3._
import chisel3.experimental._
import chisel3.util.RRArbiter
import chisel3.util._
import meowv64.cache._
import meowv64.core.CoreDef
import meowv64.core.Satp
import meowv64.core.SatpMode
import meowv64.util.FlushableSlot

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

  val MAX_SEG = coredef.vpnWidth / 9

  val state = RegInit(PTWState.idle)
  val level = RegInit(0.U(log2Ceil(MAX_SEG).W))

  val resp = Wire(new PTWResp)
  val fault = WireDefault(false.B)
  itlb.resp.bits := resp
  dtlb.resp.bits := resp
  itlb.resp.valid := false.B
  dtlb.resp.valid := false.B

  resp.fault := fault
  resp.level := level
  resp.pte := DontCare

  class TLBReq extends Bundle {
    val vpn = UInt(coredef.vpnWidth.W)
    val src = UInt(1.W) // Source
  }
  val tlbSlot = Module(new FlushableSlot(new TLBReq, false, true))
  tlbSlot.io.enq.bits.src := arbiter.io.chosen
  tlbSlot.io.enq.bits.vpn := arbiter.io.out.bits
  tlbSlot.io.enq.valid <> arbiter.io.out.valid
  tlbSlot.io.enq.ready <> arbiter.io.out.ready

  tlbSlot.io.flush.get := false.B

  val segs = VecInit((0 until MAX_SEG).map({
    case idx => {
      tlbSlot.io.deq.bits.vpn((MAX_SEG - idx) * 9 - 1, (MAX_SEG - idx - 1) * 9)
    }
  }))
  val seg = segs(level)

  tlbSlot.io.deq.nodeq()

  when(tlbSlot.io.deq.fire()) {
    itlb.resp.valid := tlbSlot.io.deq.bits.src === 0.U
    dtlb.resp.valid := tlbSlot.io.deq.bits.src === 1.U
  }

  val dcSlot = Module(new FlushableSlot(UInt(), false, true))
  dcSlot.io.flush.get := false.B
  dcSlot.io.enq.bits := dc.resp.bits
  dcSlot.io.enq.valid := dc.resp.valid
  when(dc.resp.valid) {
    assert(dcSlot.io.enq.ready)
  }
  dcSlot.io.deq.nodeq()

  switch(state) {
    is(PTWState.idle) {
      assert(!dcSlot.io.deq.valid)
      when(tlbSlot.io.deq.valid) {
        level := 0.U

        val initSeg = Wire(UInt())
        when(satp.mode === SatpMode.sv39) {
          level := 1.U // Skip level 0 in sv39
          initSeg := segs(1)
        }.otherwise {
          level := 0.U
          initSeg := segs(0)
        }

        dc.req.enq(
          DCRead.load(satp.ppn ## initSeg ## 0.U(3.W))
        ) // PTE are aligned in 64-bits
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
          when(level === (MAX_SEG - 1).U) {
            // Reached last level
            state := PTWState.idle
            resp.pte := pte
            fault := true.B
            tlbSlot.io.deq.deq()
            dcSlot.io.deq.deq()
          } otherwise {
            // Continue searching
            dc.req.enq(DCRead.load(pte.ppn ## segs(level + 1.U) ## 0.U(3.W)))
            when(dc.req.fire()) { // Wait for request to go in
              level := level + 1.U
              dcSlot.io.deq.deq()
            }
          }
        } otherwise {
          // Validate superpage alignment
          fault := pte.misaligned(level)
          state := PTWState.idle
          resp.pte := pte
          tlbSlot.io.deq.deq()
          dcSlot.io.deq.deq()
        }
      } otherwise {
        state := PTWState.idle
        resp.pte := PTE.empty
        fault := true.B
        tlbSlot.io.deq.deq()
        dcSlot.io.deq.deq()
      }
    }
  }
}
