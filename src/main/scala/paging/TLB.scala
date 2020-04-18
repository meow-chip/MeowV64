package paging

import chisel3._
import chisel3.util._
import chisel3.util.random.LFSR
import chisel3.experimental.ChiselEnum
import _root_.core.CoreDef
import _root_.core.Satp
import _root_.core.SatpMode

/**
  * Lookup privilege mode. send both if its in supervisor and SUM = true
  */
object TLBLookupMode extends ChiselEnum {
  val U, S, both = Value
}

class TLB(implicit coredef: CoreDef) extends MultiIOModule {
  val ptw = IO(new TLBExt)

  val satp = IO(Input(new Satp))

  val query = IO(new Bundle {
    val vpn = Input(UInt(coredef.vpnWidth.W))
    val ppn = Output(UInt(coredef.ppnWidth.W))

    val query = Input(Bool())
    val ready = Output(Bool())
    val fault = Output(Bool())

    val mode = Input(TLBLookupMode())
    val isModify = Input(Bool())
  })

  // TODO: check RWX permission, and MXR

  val flush = IO(Input(Bool()))

  object TLBState extends ChiselEnum {
    val idle, refill = Value
  }

  val state = RegInit(TLBState.idle)

  val storage = RegInit(VecInit(Seq.fill(coredef.TLB_SIZE)(TLBEntry.empty)))
  val hitMap = storage.map(_.hit(query.vpn))
  val hit = Mux1H(hitMap, storage)
  assert(PopCount(hitMap) <= 1.U)

  val refilling = Reg(UInt())

  val inStore = VecInit(hitMap).asUInt().orR()
  val ptwFaulted = RegInit(false.B)
  val modeMismatch = MuxLookup(query.mode.asUInt(), false.B, Seq(
    TLBLookupMode.S.asUInt -> hit.u,
    TLBLookupMode.U.asUInt -> !hit.u
  ))
  val accessFault = modeMismatch || query.isModify && !hit.d || !hit.a
  val fault = (ptwFaulted && refilling === query.vpn) || inStore && accessFault
  query.ppn := hit.fromVPN(query.vpn)
  query.fault := fault
  query.ready := (inStore || fault) && !flush

  when(inStore && accessFault) {
    query.fault := true.B
  }

  ptw.req.noenq()

  // IF can be flushed during a blocked TLB refill, so we need to ensure that we properly handles it
  // This is done by using a state machine
  when(state === TLBState.idle) {
    query.ready := false.B
    when(!flush && query.query) {
      query.ready := inStore || fault

      when(!query.ready) {
        state := TLBState.refill
        refilling := query.vpn
        ptwFaulted := false.B
      }
    }
  }.otherwise {
    query.ready := false.B
    ptw.req.enq(query.vpn)

    val random = LFSR(log2Ceil(coredef.TLB_SIZE))
    val invalids = storage.map(!_.v)
    val hasInvalid = VecInit(invalids).asUInt().orR
    val victim = Mux(
      hasInvalid,
      PriorityEncoder(invalids),
      random
    )

    val written = TLBEntry.fromPTE(refilling, ptw.level, ptw.resp)

    when(ptw.req.fire()) {
      when(!ptw.fault) {
        storage(victim) := written
      }
      ptwFaulted := ptw.fault
      state := TLBState.idle
    }
  }

  when(flush) {
    for(slot <- storage) {
      slot.v := false.B
    }
  }
}
