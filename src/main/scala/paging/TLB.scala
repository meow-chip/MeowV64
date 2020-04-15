package paging

import chisel3._
import chisel3.util._
import chisel3.util.random.LFSR
import chisel3.experimental.ChiselEnum
import _root_.core.CoreDef
import _root_.core.Satp
import _root_.core.SatpMode

class TLB(implicit coredef: CoreDef) extends MultiIOModule {
  val ptw = IO(new TLBExt)

  val satp = IO(Input(new Satp))

  val query = IO(new Bundle {
    val vpn = Input(UInt(coredef.vpnWidth.W))
    val ppn = Output(UInt(coredef.ppnWidth.W))

    val query = Input(Bool())
    val ready = Output(Bool())
  })

  // TODO: check for privilege, validness, etc...

  val storage = RegInit(VecInit(Seq.fill(coredef.TLB_SIZE)(TLBEntry.empty)))

  val hitMap = storage.map(_.hit(query.vpn))
  assert(PopCount(hitMap) <= 1.U)
  val hitResult = Mux1H(hitMap, storage.map(_.ppn))

  val inStore = VecInit(hitMap).asUInt().orR()
  query.ready := inStore && query.query
  query.ppn := hitResult

  // Refilling
  ptw.req.noenq()

  val random = LFSR(log2Ceil(coredef.TLB_SIZE))
  val invalids = storage.map(!_.v)
  val hasInvalid = VecInit(invalids).asUInt().orR
  val victim = Mux(
    hasInvalid,
    PriorityEncoder(invalids),
    random
  )

  // PTW has a latency much greater than 1, so we can use an RegNext here
  val written = TLBEntry.fromPTE(RegNext(query.vpn), ptw.level, ptw.resp)

  when(ptw.req.fire()) {
    storage(victim) := written
    // TODO: handles fault
  }
}
