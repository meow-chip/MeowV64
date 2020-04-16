package paging

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.util.RRArbiter
import _root_.core.CoreDef
import _root_.core.Satp

object PTWState extends ChiselEnum {
  val idle, reading = Value
}

class PTW(implicit coredef: CoreDef) extends MultiIOModule {
  val itlb = IO(Flipped(new TLBExt))
  val dtlb = IO(Flipped(new TLBExt))


  val satp = IO(Input(new Satp))
  val dc = IO(new PTWExt)
  dc.req.noenq()

  val arbiter = Module(new RRArbiter(UInt(coredef.vpnWidth.W), 2))
  arbiter.io.in(0) <> itlb.req
  arbiter.io.in(1) <> dtlb.req
  arbiter.io.out.nodeq()

  val state = RegInit(PTWState.idle)
  val level = RegInit(0.U)

  val MAX_SEG = coredef.vpnWidth / 9

  val segs = VecInit((0 until MAX_SEG).map({ case idx => {
    arbiter.io.out.bits((MAX_SEG-idx)*9 - 1, (MAX_SEG - idx-1)*9)
  }}))
  val seg = segs(level)

  val raddr = RegInit(0.U(coredef.PADDR_WIDTH.W))

  val resp = Wire(new PTE)
  val fault = WireDefault(false.B)
  resp := DontCare
  itlb.resp := resp
  dtlb.resp := resp
  itlb.fault := fault
  dtlb.fault := fault

  itlb.level := level
  dtlb.level := level

  switch(state) {
    is(PTWState.idle) {
      when(arbiter.io.out.valid) {
        level := 0.U
        raddr := satp.ppn ## segs(0) ## 0.U(3.W) // PTE are aligned in 64-bits
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
              fault := true.B
              arbiter.io.out.deq()
            } otherwise {
              // Continue searching
              level := level + 1.U
              raddr := pte.ppn ## segs(level + 1.U) ## 0.U(3.W)
            }
          } otherwise {
            state := PTWState.idle
            resp := pte
            arbiter.io.out.deq()
          }
        } otherwise {
          state := PTWState.idle
          resp := PTE.empty
          fault := true.B
          arbiter.io.out.deq()
        }
      }
    }
  }
}
