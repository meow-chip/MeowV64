package meowv64.frontend

import chisel3._
import chisel3.util._
import meowv64._
import meowv64.util._
import meowv64.mem._
import meowv64.config._

// TODO: we are presumably giving ICache a 3 cycle delay:
// Input -> Stage 0 | Stage 1 | Stage 2 -> Output
class InstrCache(implicit cfg: CoreConfig) extends Module {
  ////////////////////
  // IOs
  ////////////////////
  val input = IO(new Bundle {
    val s0_vaddr = Flipped(Valid(cfg.rint))
    val s1_paddr = Input(UInt(Consts.MAX_PADDR_WIDTH.W)) // When s1 is invalid, this is ignored
  })
  val output = IO(Output(new Bundle {
    val fetched = Vec(cfg.fetch_width, Consts.ibits)
    // When paused, pipeline is stalled, s0_vaddr is not accepted
    val paused = Bool()
  }))

  val data_offset_width = log2Ceil(cfg.fetch_width * Consts.INSTR_WIDTH / 8)
  // Used for assertion only
  val s0_data_subidx = input.s0_vaddr.bits(cfg.ic.offset_width - data_offset_width - 1, 0)

  assert(input.s0_vaddr.fire && input.s0_vaddr.bits(data_offset_width - 1, 0) === 0.U, "PC should be aligned")

  val kills = IO(new Bundle {
    val s1 = Input(Bool())
    val s2 = Input(Bool())
  })
  // FIXME: impls kill s2
  assert(!kills.s2)

  val mem = IO(new MemBus(cfg.membus_params(Frontend)))

  // The entire pipeline is flowing
  val flow = Wire(Bool())

  // Validity in each stage
  val s0_valid = input.s0_vaddr.valid
  val s1_valid = RegInit(false.B)
  s1_valid := Mux(
    flow, s0_valid, s1_valid && !kills.s1
  )
  val s2_valid = RegInit(false.B)
  s2_valid := Mux(
    flow, s1_valid && !kills.s1, s2_valid && !kills.s2
  )

  ////////////////////
  // States
  ////////////////////
  def FetchVec = Vec(cfg.fetch_width, Consts.ibits)
  val fetch_per_line = cfg.ic.line_width / cfg.fetch_width / Consts.INSTR_WIDTH

  // Valid matrix
  val valids = RegInit(VecInit(Seq.fill(cfg.ic.line_per_assoc)(0.U(cfg.ic.assoc_cnt.W))))

  // Tag memory
  val tags = SyncReadMem(
    cfg.ic.line_per_assoc,
    Vec(cfg.ic.assoc_cnt, UInt(cfg.ic.tag_width(Consts.MAX_PADDR_WIDTH).W)),
  )

  // Data memory
  val data = Module(new BankedMem()(BankedMemConfig(
    total_size = cfg.ic.assoc_size * cfg.ic.assoc_cnt,
    access_size = FetchVec.getWidth / 8,
    max_concurrency = 4,
    subbank_cnt = 1,
    port_cnt = 2,
  )))

  val data_read = data.ports(1)
  val data_write = data.ports(0)

  /*
  val data = Mem(
    Vec(FetchVec, cfg.ic.assoc_cnt),
    cfg.ic.line_per_assoc * fetch_per_line,
  )
  */

  /**
    * ICache operates in 4 stages:
    * - s0: send read to metadata memories
    * - s1: mux tag, get hit assoc. Re-read metadata if blocked. Send read to data memory. Victim selection happens here
    * - s2: If missed, send to MSHR. Output result
    * 
    * To maintain kill signal consistency, bubbles are not collapsed. E.g. if s1 has a bubble, and s2 stalled, s0 cannot enter s1
    */

  ////////////////
  // Stage 1
  ////////////////

  val s1_vaddr = RegEnable(input.s0_vaddr.bits, flow)
  val pre_s1_idx = cfg.ic.index(
    Mux(flow, input.s0_vaddr.bits, s1_vaddr)
  )
  // To perserve width
  val s1_valids = Reg(valids(pre_s1_idx).cloneType)
  s1_valids := valids(pre_s1_idx)

  require(log2Ceil(cfg.ic.line_per_assoc * fetch_per_line) == s0_data_subidx.getWidth + pre_s1_idx.getWidth)

  val s1_tags = tags.read(pre_s1_idx)
  val s1_hits = VecInit(s1_valids.asBools.zip(s1_tags).map({ case (valid, tag) => valid && (tag === cfg.ic.tag(input.s1_paddr)) }))
  val s1_hit = s1_hits.asUInt.orR
  val s1_victim = 0.U // TODO: get a real impl
  val s1_victim_tag = s1_tags(s1_victim)
  val s1_victimized = Wire(Bool())

  val s1_assoc = Mux(s1_hit, OHToUInt(s1_hits), s1_victim)
  // TODO: Actually this can just be OHToUInt(s1_hits), because if we miss, we have to stay in s2 for at least one cycle
  val s1_data_read_idx = s1_assoc ## cfg.ic.index(s1_vaddr) ## (cfg.ic.offset(s1_vaddr) >> log2Ceil(FetchVec.getWidth))

  ////////////////
  // Stage 2
  ////////////////

  class MSHR extends Bundle {
    val valid = Bool()
    val pending = Bool()

    val assoc = UInt(log2Ceil(cfg.ic.assoc_cnt).W)
    val idx = UInt(cfg.ic.index_width.W)
    val subidx = UInt(log2Ceil(cfg.ic.line_width / FetchVec.getWidth).W)
    val cnt = UInt(log2Ceil(cfg.ic.line_width / FetchVec.getWidth).W)
  }

  val mshr = RegInit({
    val m = Wire(new MSHR)
    m := DontCare
    m.valid := false.B
    m
  })

  val s2_exit = Wire(Bool())

  val s2_hit = RegEnable(s1_hit && !s1_victimized, flow)
  val s2_assoc = RegEnable(s1_assoc, flow)
  val s2_paddr = RegEnable(input.s1_paddr, flow)

  val s2_idx = cfg.ic.index(s2_paddr)
  val s2_subidx = cfg.ic.offset(s2_paddr) >> log2Ceil(FetchVec.getWidth)
  val s2_data_read_idx = s2_assoc ## s2_idx ## s2_subidx
  val s2_victim_tag = RegEnable(s1_victim_tag, flow)

  s1_victimized := cfg.ic.tag(input.s1_paddr) === s2_victim_tag && cfg.ic.index(s2_paddr) === cfg.ic.index(s1_vaddr)

  val pre_s2_data_read_idx = Mux(flow, s1_data_read_idx, s2_data_read_idx)

  // TODO: To save power, only enable if pre_s2_valid
  data_read.req.valid := s2_exit
  // Always read from pre_s1 to avoid comb loop
  data_read.req.bits.idx := s1_data_read_idx.asUInt
  data_read.req.bits.we := false.B
  data_read.req.bits.sbe := -1.S.asUInt
  data_read.req.bits.wdata := DontCare

  val s2_data_holder = Reg(data_read.readout.cloneType)
  val s2_data_raw = Mux(RegNext(flow), data_read.readout, s2_data_holder)
  // Forwarded at MSHR refill
  val s2_data = Wire(s2_data_raw.cloneType)
  s2_data_holder := s2_data

  val s2_mshr_alloc = Wire(DecoupledIO(new Bundle {}))
  s2_mshr_alloc.valid := !s2_hit
  s2_mshr_alloc.ready := !mshr.valid
  when(s2_mshr_alloc.fire) {
    assert(!flow)

    // Actually this only need to depend on ready, becuase if not valid, we are already hit, so or-ing is a no-op
    valids(cfg.ic.index(s2_paddr)) := valids(cfg.ic.index(s2_paddr)) | UIntToOH(s2_assoc, cfg.ic.assoc_cnt)

    // Transition into hit, next cycle s2_blocked_by_mshr should be asserted
    s2_hit := true.B

    assert(mshr.cnt === 0.U)
    mshr.valid := true.B
    mshr.pending := true.B
    mshr.assoc := s2_assoc
    mshr.idx := s2_idx
    mshr.subidx := s2_subidx
  }
  // No need to forward to s1, because we will at least stay here for at least one more cycle
  when(s2_mshr_alloc.fire) {
    tags.write(cfg.ic.index(s2_paddr), VecInit(Seq.fill(cfg.ic.assoc_cnt)(cfg.ic.tag(s2_paddr))), UIntToOH(s2_assoc, cfg.ic.assoc_cnt).asBools)
  }
  val s2_blocked_by_mshr = mshr.idx === s2_idx && mshr.cnt > (s2_subidx - mshr.idx)

  s2_exit := !s2_blocked_by_mshr && s2_hit

  output.fetched := s2_data.asTypeOf(FetchVec)
  output.paused := !flow

  /**
    * When all of the following is true, flow is asserted:
    * - Data memory read is accepted
    * - s2 not blocked by MSHR
    * - s2 hit or already allocated mshr
    */
  flow := (
    (data_read.req.ready || !data_read.req.valid)
    && s2_exit
  )
  // If missed, need to send. Essentially !sent

  ////////////////////
  // Refilling
  ////////////////////
  mem.cmd.bits.id := 0.U
  mem.cmd.bits.addr := s2_paddr
  mem.cmd.valid := mshr.valid && mshr.pending
  when(mshr.pending) {
    assert(mshr.cnt === 0.U)
  }
  when(mem.cmd.fire) {
    mshr.pending := false.B
  }

  val uplink_data = mem.uplink.bits.data.asTypeOf(FetchVec)
  val uplink_write = Bool()
  mem.uplink.ready := true.B

  when(mem.uplink.fire) {
    mshr.cnt := mshr.cnt + 1.U
    when(mshr.cnt.andR) {
      // Last transfer
      mshr.valid := false.B
    }
  }

  // TODO: Do we need to make I$ single port?
  require(s0_data_subidx.getWidth == mshr.cnt.getWidth)
  val data_write_idx = (mshr.assoc ## mshr.idx ## (mshr.subidx + mshr.cnt)).asUInt

  data_write.req.valid := mem.uplink.fire
  when(data_write.req.valid) {
    assert(data_write.req.ready)
  }

  data_write.req.bits.idx := data_write_idx
  data_write.req.bits.sbe := -1.S.asUInt
  data_write.req.bits.we := true.B
  data_write.req.bits.wdata := mem.uplink.bits.data

  // We don't need to depend on valid here, because if invalid, cnt is not incremented, and s2 cannot exit in the next cycle
  s2_data := Mux(RegNext(data_read.req.bits.idx === data_write_idx), RegNext(data_write.req.bits.wdata), s2_data_raw)
}