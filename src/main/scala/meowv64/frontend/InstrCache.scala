package meowv64.frontend

import spinal.core._
import spinal.lib._
import meowv64.config.CoreConfig
import meowv64.config.Consts
import meowv64.MemBus
import meowv64.MemBusParams

// TODO: we are presumably giving ICache a 3 cycle delay:
// Input -> Stage 0 | Stage 1 | Stage 2 -> Output
class InstrCache(implicit cfg: CoreConfig) extends Component {
  ////////////////////
  // IOs
  ////////////////////
  val input = new Bundle {
    val s0_vaddr = slave Flow (cfg.rint)
    val s1_paddr = in (cfg.rint) // When s1 is invalid, this is ignored
  }
  val output = new Bundle {
    val fetched = master Flow (FetchVec)
    // When paused, pipeline is stalled, s0_vaddr is not accepted
    val paused = out Bool()
  }

  val data_offset_width = cfg.fetch_width * Consts.INSTR_WIDTH
  val s0_data_subidx = input.s0_vaddr.payload(0, (cfg.ic.offset_width - data_offset_width) bits)

  assert(input.s0_vaddr.fire && input.s0_vaddr.payload(0, data_offset_width bits) === U(0), "PC should be aligned")

  val kills = new Bundle {
    val s1 = in Bool()
    val s2 = in Bool()
  }
  // FIXME: impls kill s2
  assert(!kills.s2)

  val membus = master (new MemBus(MemBusParams(
    addr_width = cfg.xlen,
    data_width = cfg.xlen,
    id_width = 4,
  ), true))

  ////////////////////
  // States
  ////////////////////
  def FetchVec = Vec(Consts.ibits, cfg.fetch_width)
  val fetch_per_line = cfg.ic.line_width * 8 / cfg.fetch_width

  // Valid matrix
  val valids = RegInit(Vec(
    B(0, cfg.ic.assoc_cnt bits),
    cfg.ic.line_per_assoc,
  ))

  // Tag memory
  val tags = Mem(
    Vec(UInt(cfg.ic.tag_width(cfg.fetch_width) bits), cfg.ic.assoc_cnt),
    cfg.ic.line_per_assoc,
  )

  // Data memory
  val data = Mem(
    Vec(FetchVec, cfg.ic.assoc_cnt),
    cfg.ic.line_per_assoc * fetch_per_line,
  )

  val s0_flow = Bool()
  val s0_just_flowed = RegNext(s0_flow)
  val s1_flow = Bool()
  val s2_flow = Bool()

  ////////////////
  // Stage 0 + Stage 1
  ////////////////

  /**
    * Stage 0 sends reads to all memory
    */
  val s0_idx = cfg.ic.index(input.s0_vaddr.payload)
  val s0_valids = valids(s0_idx)

  require(log2Up(cfg.ic.line_per_assoc * fetch_per_line) == s0_data_subidx.getBitsWidth + s0_idx.getBitsWidth)

  val s1_vaddr = RegNextWhen(input.s0_vaddr, s0_flow)
  val s1_tags = Vec(UInt(cfg.ic.tag_width(cfg.xlen) bits), cfg.ic.assoc_cnt)
  val s1_data = Vec(FetchVec, cfg.ic.assoc_cnt)
  s1_tags := Mux(s0_just_flowed, tags.readSync(s0_idx), RegNext(s1_tags))
  s1_data := Mux(s0_just_flowed, data.readSync((s0_idx ## s0_data_subidx).as(UInt())), RegNext(s1_data))
  val s1_valids = RegNextWhen(s0_valids, s0_flow)

  val s1_data_idx = RegNextWhen((s0_idx ## s0_data_subidx).as(UInt()), s0_flow)
  // Only for assertion
  val s1_tag_idx = RegNextWhen(s0_idx, s0_flow)

  val s1_valid = RegNextWhen(input.s0_vaddr.fire, s0_flow)
  s0_flow := s1_flow || !s1_valid || kills.s1

  val s1_hits = Vec(s1_valids.asBools.zip(s1_tags).map({ case (valid, tag) => valid && (tag === cfg.ic.tag(input.s1_paddr)) }))
  val s1_hit = s1_hits.orR
  val s1_muxed = MuxOH(s1_hits, s1_data)

  // S2 -> S1 forward signals
  val s1_forwarded = Reg(FetchVec)
  val s1_forwarded_valid = RegNextWhen(False, s0_flow)
  val s1_forward = Bool()

  ////////////////
  // Stage 2
  ////////////////
  val uplink_data = membus.uplink.data.as(FetchVec)

  val s2_valid = RegNextWhen(s1_valid && !kills.s1, s1_flow)
  val s2_hit = RegNextWhen(s1_forwarded_valid || s1_forward || s1_hit, s1_flow)
  val s2_data = RegNextWhen(PriorityMux(Seq(
    s1_forwarded_valid -> s1_forwarded,
    s1_forward -> uplink_data,
    True -> s1_muxed,
  )), s1_flow)
  val s2_paddr = RegNextWhen(input.s1_paddr, s1_flow)
  // If missed, need to send. Essentially !sent
  val s2_pending = RegInit(True)

  val cnt = Reg(UInt(log2Up(cfg.ic.line_width / FetchVec.getBitsWidth) bits))

  membus.cmd.payload.id := 0
  membus.cmd.payload.addr := s2_paddr
  membus.cmd.valid := s2_valid && !s2_hit && s2_pending

  // TODO: Do we need to make I$ single port?
  require(s0_data_subidx.getBitsWidth == cnt.getBitsWidth)
  val write_tag = cfg.ic.tag(s2_paddr)
  val write_idx = cfg.ic.index(s2_paddr)
  val write_data_idx = (write_idx ## (s0_data_subidx + cnt)).as(UInt())

  val write_assoc_map: Bits = 1 // FIXME: get a real victim impl
  val first_resp = Reg(FetchVec)
  val uplink_last = membus.uplink.valid && cnt.andR

  data.write(
    write_data_idx,
    Vec((0 to cfg.ic.assoc_cnt).map(_ => uplink_data)),
    membus.uplink.valid,
    write_assoc_map,
  )

  tags.write(
    write_idx,
    Vec((0 to cfg.ic.assoc_cnt).map(_ => write_tag)),
    uplink_last,
    write_assoc_map,
  )

  when(s2_valid && s2_pending && !s2_hit) {
    valids(write_idx) := valids(write_idx) & ~write_assoc_map
  }

  when(uplink_last) {
    assert(!s2_pending)
    valids(write_idx) := valids(write_idx) | write_assoc_map
  }

  when(s1_flow) {
    s2_pending := True 
    cnt := 0
  }

  when(membus.cmd.ready) {
    assert(s2_pending)
    assert(!s1_flow)
    s2_pending := False
  }

  when(membus.uplink.valid) {
    assert(!s1_flow)
    cnt := cnt + 1

    when(cnt === 0) {
      first_resp := uplink_data
    }
  }

  membus.uplink.ready := True

  s1_flow := s2_flow || !s2_valid
  s2_flow := s2_hit || uplink_last

  output.fetched.payload := Mux(s2_hit, s2_data, first_resp)
  output.fetched.valid := s2_flow
  output.paused := !s0_flow

  // Refill -> s1 forward
  s1_forward := (
    s1_valid
      && write_data_idx === s1_data_idx
      && cfg.ic.tag(s2_paddr) === cfg.ic.tag(input.s1_paddr)
      && membus.uplink.valid
  )

  when(s1_forward) {
    s1_forwarded := uplink_data
    s1_forwarded_valid := True
  }
}