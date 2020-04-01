package paging

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.util.RRArbiter
import _root_.core.CoreDef

object PTWState extends ChiselEnum {
  val idle, reading = Value
}

class PTW(implicit coredef: CoreDef) extends MultiIOModule {
  val itlb = IO(Flipped(new TLBExt))
  val dtlb = IO(Flipped(new TLBExt))

  val satp = IO(Input(UInt(coredef.ADDR_WIDTH.W)))
  val dc = IO(new PTWExt)

  val arbiter = Module(new RRArbiter(UInt(coredef.vpnWidth.W), 2))
  arbiter.io.in(0) <> itlb.req
  arbiter.io.in(1) <> dtlb.req

  val state = RegInit(PTWState.idle)
  val level = RegInit(0.U)

  val MAX_SEG = coredef.vpnWidth / 9

  val segs = VecInit((0 until MAX_SEG).map({ case idx => {
    arbiter.io.out.bits((MAX_SEG-idx)*9 - 1, (MAX_SEG - idx-1)*9)
  }}))
  val seg = segs(level)

  val raddr = RegInit(UInt(coredef.ADDR_WIDTH.W))

  val resp = Wire(new TLBEntry)
  resp := DontCare
  itlb.resp := resp
  dtlb.resp := resp

  itlb.level := level
  dtlb.level := level

  switch(state) {
    is(PTWState.idle) {
      dc.req.noenq()

      when(arbiter.io.out.valid) {
        level := 0.U
        raddr := (satp +% segs(0)) << 3 // PTE are aligned in 64-bits
        state := PTWState.reading
      }
    }

    is(PTWState.reading) {
      dc.req.enq(raddr)

      when(dc.req.fire()) {
        // Responded

        val pte = dc.resp.asTypeOf(new PTE)

        when(pte.valid) {
          when(pte.intermediate) {
            when(level === (MAX_SEG-1).U) {
              // Reached last level
              state := PTWState.idle
              resp := pte
              arbiter.io.out.deq()
            } otherwise {
              // Continue searching
              raddr := pte.ppn << 12 // PAGE_SIZE
            }
          } otherwise {
            state := PTWState.idle
            resp := pte
            arbiter.io.out.deq()
          }
        } otherwise {
          state := PTWState.idle
          resp := PTE.empty
          arbiter.io.out.deq()
        }
      }
    }
  }
}
