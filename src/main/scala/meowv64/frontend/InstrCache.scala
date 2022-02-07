package meowv64.frontend

import spinal.core._
import spinal.lib._
import meowv64._
import meowv64.util._
import meowv64.mem._
import meowv64.config._

// TODO: we are presumably giving ICache a 3 cycle delay:
// Input -> Stage 0 | Stage 1 | Stage 2 -> Output
class InstrCache(implicit cfg: CoreConfig) extends Component {
  ////////////////////
  // IOs
  ////////////////////
  val input = new Bundle {
    val s0_vaddr = slave Flow (cfg.rint)
    val s1_paddr = in UInt(Consts.MAX_PADDR_WIDTH bits) // When s1 is invalid, this is ignored
  }
  val output = new Bundle {
    val fetched = out(FetchVec)
    // When paused, pipeline is stalled, s0_vaddr is not accepted
    val paused = Bool()
  }

  val data_offset_width = cfg.fetch_width * Consts.INSTR_WIDTH
  // Used for assertion only
  val s0_data_subidx = input.s0_vaddr.payload(0, (cfg.ic.offset_width - data_offset_width) bits)

  assert(input.s0_vaddr.fire && input.s0_vaddr.payload(0, data_offset_width bits) === U(0), "PC should be aligned")

  val kills = new Bundle {
    val s1 = in Bool()
    val s2 = in Bool()
  }
  // FIXME: impls kill s2
  assert(!kills.s2)

  val mem = master (new MemBus(cfg.membus_params(Frontend)))

  // The entire pipeline is flowing
  val flow = Bool()

  // Validity in each stage
  val s0_valid = input.s0_vaddr.valid
  val s1_valid = RegInit(False)
  s1_valid := Mux(
    flow, s0_valid, s1_valid && !kills.s1
  )
  val s2_valid = RegInit(False)
  s2_valid := Mux(
    flow, s1_valid && !kills.s1, s2_valid && !kills.s2
  )

  ////////////////////
  // States
  ////////////////////
  def FetchVec = Vec(Consts.ibits, cfg.fetch_width)
  val fetch_per_line = cfg.ic.line_width / cfg.fetch_width

  // Valid matrix
  val valids = RegInit(Vec(
    B(0, cfg.ic.assoc_cnt bits),
    cfg.ic.line_per_assoc,
  ))

  // Tag memory
  val tags = Mem(
    Vec(UInt(cfg.ic.tag_width(Consts.MAX_PADDR_WIDTH) bits), cfg.ic.assoc_cnt),
    cfg.ic.line_per_assoc,
  )

  // Data memory
  val data = new BankedMem("L1I Data")(BankedMemConfig(
    total_size = cfg.ic.assoc_size * cfg.ic.assoc_cnt,
    access_size = FetchVec.getBitsWidth / 8,
    max_concurrency = 4,
    subbank_cnt = 1,
    port_cnt = 2,
  ))

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

  val s1_vaddr = RegNextWhen(input.s0_vaddr.payload, flow)
  val pre_s1_idx = cfg.ic.index(
    Mux(flow, input.s0_vaddr.payload, s1_vaddr)
  )
  val s1_valids = RegNext(valids(pre_s1_idx))

  require(log2Up(cfg.ic.line_per_assoc * fetch_per_line) == s0_data_subidx.getBitsWidth + pre_s1_idx.getBitsWidth)

  val s1_tags = tags.readSync(pre_s1_idx)
  val s1_hits = Vec(s1_valids.asBools.zip(s1_tags).map({ case (valid, tag) => valid && (tag === cfg.ic.tag(input.s1_paddr)) }))
  val s1_hit = s1_hits.orR
  val s1_victim = U(0) // TODO: get a real impl
  val s1_victim_tag = s1_tags(s1_victim)
  val s1_victimized = Bool()

  val s1_assoc = Mux(s1_hit, OHToUInt(s1_hits), s1_victim)
  // TODO: Actually this can just be OHToUInt(s1_hits), because if we miss, we have to stay in s2 for at least one cycle
  val s1_data_read_idx = s1_assoc ## cfg.ic.index(s1_vaddr) ## (cfg.ic.offset(s1_vaddr) >> log2Up(FetchVec.getBitsWidth))

  ////////////////
  // Stage 2
  ////////////////

  class MSHR extends Bundle {
    val valid = Bool()
    val pending = Bool()

    val assoc = UInt(log2Up(cfg.ic.assoc_cnt) bits)
    val idx = UInt(cfg.ic.index_width bits)
    val subidx = UInt(log2Up(cfg.ic.line_width / FetchVec.getBitsWidth) bits)
    val cnt = UInt(log2Up(cfg.ic.line_width / FetchVec.getBitsWidth) bits)
  }

  val mshr = Reg(new MSHR)
  mshr.valid init(False)

  val s2_hit = RegNextWhen(s1_hit && !s1_victimized, flow)
  val s2_assoc = RegNextWhen(s1_assoc, flow)
  val s2_paddr = RegNextWhen(input.s1_paddr, flow)

  val s2_idx = cfg.ic.index(s2_paddr)
  val s2_subidx = cfg.ic.offset(s2_paddr) >> log2Up(FetchVec.getBitsWidth)
  val s2_data_read_idx = s2_assoc ## s2_idx ## s2_subidx
  val s2_victim_tag = RegNextWhen(s1_victim_tag, flow)

  s1_victimized := cfg.ic.tag(input.s1_paddr) === s2_victim_tag && cfg.ic.index(s2_paddr) === cfg.ic.index(s1_vaddr)

  val pre_s2_data_read_idx = Mux(flow, s1_data_read_idx, s2_data_read_idx)

  // TODO: To save power, only enable if pre_s2_valid
  data_read.req.valid := True
  data_read.req.payload.idx := pre_s2_data_read_idx.asUInt
  data_read.req.payload.we := False
  data_read.req.payload.sbe := B(1)
  data_read.req.payload.wdata.assignDontCare()

  val s2_mshr_alloc = Stream(NoData)
  s2_mshr_alloc.valid := !s2_hit
  s2_mshr_alloc.ready := !mshr.valid
  when(s2_mshr_alloc.fire) {
    assert(!flow)

    // Actually this only need to depend on ready, becuase if not valid, we are already hit, so or-ing is a no-op
    valids(cfg.ic.index(s2_paddr)) := valids(cfg.ic.index(s2_paddr)) | UIntToOh(s2_assoc, cfg.ic.assoc_cnt)

    // Transition into hit, next cycle s2_blocked_by_mshr should be asserted
    s2_hit := True

    assert(mshr.cnt === 0)
    mshr.valid := True
    mshr.pending := True
    mshr.assoc := s2_assoc
    mshr.idx := s2_idx
    mshr.subidx := s2_subidx
  }
  // No need to forward to s1, because we will at least stay here for at least one more cycle
  tags.write(cfg.ic.index(s2_paddr), Vec.fill(cfg.ic.assoc_cnt)(cfg.ic.tag(s2_paddr)), s2_mshr_alloc.fire, UIntToOh(s2_assoc, cfg.ic.assoc_cnt))
  val s2_blocked_by_mshr = mshr.idx === s2_idx && mshr.cnt > (s2_subidx - mshr.idx)

  // Forwarded at MSHR refill
  val s2_data = FetchVec.clone()

  output.fetched := s2_data
  output.paused := !flow

  /**
    * When all of the following is true, flow is asserted:
    * - Data memory read is accepted
    * - s2 not blocked by MSHR
    * - s2 hit or already allocated mshr
    */
  flow := (
    (data_read.req.ready || !data_read.req.valid)
    && !s2_blocked_by_mshr
    && s2_hit
  )
  // If missed, need to send. Essentially !sent

  ////////////////////
  // Refilling
  ////////////////////
  mem.cmd.payload.id := 0
  mem.cmd.payload.addr := s2_paddr
  mem.cmd.valid := mshr.valid && mshr.pending
  when(mshr.pending) {
    assert(mshr.cnt === 0)
  }
  when(mem.cmd.fire) {
    mshr.pending := False
  }

  val uplink_data = mem.uplink.data.as(FetchVec)
  val uplink_write = Bool()
  mem.uplink.ready := True

  when(mem.uplink.fire) {
    mshr.cnt := mshr.cnt + 1
    when(mshr.cnt.andR) {
      // Last transfer
      mshr.valid := False
    }
  }

  // TODO: Do we need to make I$ single port?
  require(s0_data_subidx.getBitsWidth == mshr.cnt.getBitsWidth)
  val data_write_idx = (mshr.assoc ## mshr.idx ## (mshr.subidx + mshr.cnt)).as(UInt())

  data_write.req.valid := mem.uplink.fire
  when(data_write.req.valid) {
    assert(data_write.req.ready)
  }

  data_write.req.payload.idx := data_write_idx
  data_write.req.sbe := B(1)
  data_write.req.we := True
  data_write.req.wdata := mem.uplink.payload.data

  // We don't need to depend on valid here, because if invalid, cnt is not incremented, and s2 cannot exit in the next cycle
  s2_data := Mux(RegNext(data_read.req.payload.idx === data_write_idx), RegNext(data_write.req.wdata), data_read.readout).as(FetchVec)
}