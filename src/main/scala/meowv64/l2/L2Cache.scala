package meowv64.l2

import chisel3._
import chisel3.util._
import meowv64.mem._
import meowv64._
import meowv64.config._
import meowv64.util._

/**
  * This is a stub. You can extend it by feeding meow
  */

class L2Cache(implicit cfg: MulticoreConfig) extends Module {
  require(cfg.l2.base.inst_cnt == 1) // TODO: impl banking

  val internal_bus = cfg.cores.map(core => new Bundle {
    val frontend = Flipped(new MemBus(core.membus_params(Frontend)))
    val backend = Flipped(new MemBus(core.membus_params(Backend)))
  })
  val external_bus = new MemBus(cfg.membus_params(L2))

  val banks = for(i <- 0 to cfg.l2.base.inst_cnt) yield new L2Inst(i)
  banks(0).external_bus <> external_bus
  for((l, r) <- banks(0).internal_bus.zip(internal_bus)) l <> r
}

class PendingInv extends Bundle {
  val valid = Bool()

  val is_invalidation = Bool() // If not, is 
  val sent = Bool() // If true, already sent to master, but has pending data write

  val addr = UInt(Consts.MAX_PADDR_WIDTH.W)
}

class PendingWrite(implicit cfg: MulticoreConfig) extends Bundle with MatchedData[UInt] {
  val valid = Bool()

  val addr = UInt(Consts.MAX_PADDR_WIDTH.W)
  val assoc = UInt(log2Ceil(cfg.l2.base.assoc_cnt).W)
  val idx = UInt(cfg.l2.base.index_width.W)
  val subidx = UInt(log2Ceil(cfg.l2.base.line_width / cfg.cores(0).membus_params(Frontend).data_width).W)
  val cnt = UInt(log2Ceil(cfg.l2.base.line_width / cfg.cores(0).membus_params(Frontend).data_width).W)

  val associated_inv = UInt(log2Ceil(cfg.l2.max_pending_inv).W)

  def matched(matcher: UInt): Bool = cfg.l2.base.tag(addr) === cfg.l2.base.tag(matcher) && cfg.l2.base.index(addr) === cfg.l2.base.index(matcher)
}

class PendingReadHead(implicit cfg: MulticoreConfig) extends Bundle {
  val cnt = UInt(log2Ceil(cfg.l2.base.line_width / cfg.cores(0).membus_params(Frontend).data_width).W)
}

class PendingRead(implicit cfg: MulticoreConfig) extends Bundle with MatchedData[UInt] {
  val addr = UInt(Consts.MAX_PADDR_WIDTH.W)

  val assoc = UInt(log2Ceil(cfg.l2.base.assoc_cnt).W)
  val idx = UInt(cfg.l2.base.index_width.W)
  val subidx = UInt(log2Ceil(cfg.l2.base.line_width / cfg.cores(0).membus_params(Frontend).data_width).W)

  def matched(matcher: UInt): Bool = cfg.l2.base.tag(addr) === cfg.l2.base.tag(matcher) && cfg.l2.base.index(addr) === cfg.l2.base.index(matcher)
}

/**
  * Miss status holding register + Coherence conflict holding register. When an request causes a miss (a read, a occupy with data),
  * or a coherence conflict, a MSCCHR is allocated which is in charge of driving writeback and refilling.
  */
class L2MSCCHR(implicit cfg: MulticoreConfig) extends Bundle {
  val data_size = cfg.membus_params(L2).data_width / 8
  val addr_width = cfg.membus_params(L2).addr_width

  val valid = Bool()

  // For replying
  val req_port = UInt(log2Ceil(cfg.cores.length * 2).W)
  val req_id = UInt(cfg.cores(0).membus_params(Frontend).id_width.W)

  // Master is reading/writing victim. Can send writeback / refill request, but cannot accept data yet, nor cannot invalidate master
  // TODO: how to wakeup?
  val victim_ongoing_rw = UInt(log2Ceil(cfg.cores.length * (2 * cfg.l2.max_pending_read + cfg.l2.max_pending_write)).W)

  // TODO: only store one idx##offset for addr_r and addr_w
  val addr_r = UInt(addr_width.W) // Keyword first
  val addr_w = UInt(addr_width.W) // Also keyword first

  // Metadata idx can be fetched from req
  val assoc = UInt(log2Ceil(cfg.l2.base.assoc_cnt).W)
  def idx = cfg.l2.base.index(addr_r)
  def subidx = cfg.l2.base.offset(addr_r) >> log2Ceil(cfg.cores(0).membus_params(Frontend).data_width) // data_width = access_size

  val mask_i = Bits(cfg.cores.length.W)
  
  val has_i_data = Bool()

  val cnt_i = UInt(log2Ceil(cfg.l2.base.line_size / data_size).W)
  val cnt_r = UInt(log2Ceil(cfg.l2.base.line_size / data_size).W)
  val cnt_w = UInt(log2Ceil(cfg.l2.base.line_size / data_size).W)

  // Having to send request to master
  val pending_r = Bool()
  val pending_w = Bool()
  val pending_i = Bits(cfg.cores.length.W)

  // Waiting response from master
  val waiting_w = Bool()
  val waiting_r = Bool()
  val waiting_i = Bits(cfg.cores.length.W)

  // valid \implies (ongoing_rw \implies pending_w)
  // BTW, pending_x is !sent_x && has_x
  assert(!valid || (!victim_ongoing_rw.orR || pending_w))
}

class L2Metadata(implicit val cfg: MulticoreConfig) extends Bundle {
  val valid = Bool()
  val dirty = Bool()
  val occupation = Bits(cfg.cores.length.W)
  val master_dirty = Bool()
  val tag = Bits(cfg.l2.base.tag_width(Consts.MAX_PADDR_WIDTH).W)
}

class L2Inst(inst_id: Int)(implicit cfg: MulticoreConfig) extends Module {
  val internal_bus = cfg.cores.map(core => new Bundle {
    val frontend = Flipped(new MemBus(core.membus_params(Frontend)))
    val backend = Flipped(new MemBus(core.membus_params(Backend)))
  })
  // TODO: assert they are homogeneous
  val external_bus = new MemBus(cfg.membus_params(L2))

  val src: Seq[MemBus] = internal_bus.flatMap(core => Seq(core.frontend, core.backend))

  // Memories
  val metadata = SyncReadMem(cfg.l2.base.line_per_assoc, Vec(cfg.l2.base.assoc_cnt, new L2Metadata))
  metadata.suggestName(s"L2.$inst_id.Metadata")
  val metadata_write_idx = UInt(cfg.l2.base.index_width.W)
  val metadata_write_payload = new L2Metadata
  val metadata_write_mask = Bits(cfg.l2.base.assoc_cnt.W)
  metadata.write(
    metadata_write_idx,
    VecInit(Seq.fill(cfg.l2.base.assoc_cnt)({ metadata_write_payload })),
    mask = metadata_write_mask.asBools,
  )

  implicit val banked_mem_cfg = BankedMemConfig(
    total_size = cfg.l2.base.assoc_size,
    access_size = internal_bus(0).frontend.params.data_width / 8,
    max_concurrency = 4, subbank_cnt = 1,

    /**
      * Ports in decreasing priority:
      * - writeback
      * - refill
      * - invresp
      * - write
      * - read
      */
    port_cnt = 5
  )
  val data = new BankedMem(s"L2.$inst_id.Data")

  // Banked memory ports
  val writeback_req = DecoupledIO(new BankedMemReq)
  val refill_req = DecoupledIO(new BankedMemReq)
  val invresp_req = DecoupledIO(new BankedMemReq)
  val write_req = DecoupledIO(new BankedMemReq)
  val read_req = DecoupledIO(new BankedMemReq)

  val banked_req = Seq(writeback_req, refill_req, invresp_req, write_req, read_req)
  (data.ports, banked_req).zipped.foreach(_.req <> _)

  val writeback_resp = data.ports(0)
  // val refill_resp = data.ports(1) // Ignored
  val invresp_resp = data.ports(2)
  val write_resp = data.ports(3)
  val read_resp = data.ports(4)

  // Pendings
  val pending_writes = src.flatMap(bus => (
    if(bus.params.bus_type.with_write)
      Some(new MatchingQueue(new PendingWrite, UInt(Consts.MAX_PADDR_WIDTH.W), cfg.l2.max_pending_write))
    else None
  ))
  // Unfinished invalidates must corresponds to MSCCHR, so there is no need to match / wakeup other MSCCHRs
  val pending_invs = src.flatMap(bus =>
    if(bus.params.bus_type.with_coherence)
      Some(new Queue(new PendingInv, cfg.l2.max_pending_inv))
    else
      None
  )
  val pending_read_heads = src.map(_ => Reg(new PendingReadHead))
  val pending_reads = src.map(_ => new MatchingQueue(new PendingRead, UInt(Consts.MAX_PADDR_WIDTH.W), cfg.l2.max_pending_read))
  require(pending_writes.length == cfg.cores.length)

  // Wakeups from read/write port
  val wakeup_reads = src.map(_ => Valid(UInt(Consts.MAX_PADDR_WIDTH.W)))
  val wakeup_writes = src.flatMap(e => if(e.params.bus_type.with_write) Some(Valid(UInt(Consts.MAX_PADDR_WIDTH.W))) else None)

  class TargetedPendingRead extends PendingRead {
    val port_idx = UInt(log2Ceil(src.length).W)
  }
  class TargetedPendingWrite extends PendingWrite {
    val port_idx = UInt(log2Ceil(src.length).W)
  }

  val direct_read_req = DecoupledIO(new TargetedPendingRead)
  val mscchr_read_req = DecoupledIO(new TargetedPendingRead)

  val direct_write_req = DecoupledIO(new TargetedPendingWrite)

  // TODO: queue
  val cmd_arb = new RRArbiter(new MemBusCmd(src(0).params), src.length)
  (cmd_arb.io.in, src).zipped.foreach(_ <> _.cmd)
  val downlink_arb = new RRArbiter(new MemBusDownlink(src(0).params), src.length / 2)
  (downlink_arb.io.in, src.flatMap(s => Option(s.downlink))).zipped.foreach(_ <> _)
  val resp_arb = new RRArbiter(new MemBusDownlink(src(0).params), src.length / 2)
  (resp_arb.io.in, src.flatMap(s => Option(s.resp))).zipped.foreach(_ <> _)

  // Ack channel doesn't need to be arbitered

  // MSHRs
  val mscchrs = RegInit(VecInit(Seq.fill(cfg.l2.mscchr_cnt)({
    val m = new L2MSCCHR
    m := DontCare
    m.valid := false.B
    m.cnt_i := 0.U
    m.cnt_w := 0.U
    m.cnt_r := 0.U
    m
  })))

  ////////////////////
  // cmd channel
  ////////////////////

  /**
    * CMD channel messages are processed in 3 stages
    * - s0: Read MSCCHR and metadata.
    * - s1: Block by MSCCHR(done_r = false.B). Also block if the previous request is at the same line. Re-read MSCCHR / metadata if blocked.
    *   Tag comparision, hit detection. Metadata / MSCCHR allocation from s2 forwarded. Victim selection happens here.
    * - s2: Edit directory. If miss / coherence conflict, send to MSCCHR. Otherwise, send directly to r/w buffer.
    */
  
  // s0 is essentially a no-op
  // int stands for interface
  val cmd_s0_s1_int = DecoupledIO(new Bundle {
    val req = new MemBusCmd(cmd_arb.io.out.bits.params)
    val chosen = UInt(log2Ceil(src.length).W)
  })
  cmd_s0_s1_int.valid <> cmd_arb.io.out.valid
  cmd_s0_s1_int.ready <> cmd_arb.io.out.ready
  cmd_s0_s1_int.bits.req <> cmd_arb.io.out.bits
  cmd_s0_s1_int.bits.chosen <> cmd_arb.io.chosen

  val cmd_s1_s2_int = DecoupledIO(new Bundle {
    val req = new MemBusCmd(cmd_arb.io.out.bits.params)
    val chosen = UInt(log2Ceil(src.length).W)
    val hit = Bool()
    val assoc = UInt(log2Ceil(cfg.l2.base.assoc_cnt).W)
    // Metadata of the hit assoc, or the selected victim
    val metadata = new L2Metadata
  })

  val cmd_s1_blocked_by_s2 = Bool()

  val cmd_s1_valid = RegInit(false.B)
  val cmd_s1_req = Reg(new MemBusCmd(cmd_arb.io.out.bits.params))
  val cmd_s1_chosen = Reg(UInt(log2Ceil(src.length).W))
  cmd_s0_s1_int.ready := !cmd_s1_valid || cmd_s1_s2_int.ready

  when(cmd_s1_s2_int.fire) {
    cmd_s1_valid := cmd_s0_s1_int.fire
    cmd_s1_req := cmd_s0_s1_int.bits.req
    cmd_s1_chosen := cmd_s0_s1_int.bits.chosen
  }

  // TODO: multiple requests in MSCCHR, do not block by MSCCHR
  /** TODO: Also, maybe we can do a retry queue for each MSCCHR. The entire flow should be as followed:
    * Each request has a retry_cnt. If it sees a non-conflicting MSCCHR, merge into that MSCCHR. If it sees a conflicting MSCCHR and its
    * retry_cnt hasn't reached the max count, append to the retry queue.
    * 
    * After finalizing the MSCCHR, pop all requests from the retry queue and retry in the cmd pipeline.
    * Need to be careful that the main pipeline may run out of MSCCHR and block, so we need a sufficiently large drain
    * to consume the retry queue. This buffer will have length mscchr_cnt * retry_queue_depth + 2 (for the 2 requests already in the pipeline)
    * 
    * This buffer takes higher precedence over the masters to avoid deadlock
    * 
    * The pipeline should only block if the conflicting MSCCHR run out of retry queue space, or the request reaches max retry_cnt
    */

  val cmd_pre_s1_addr = Mux(cmd_s0_s1_int.fire, cmd_s0_s1_int.bits.req.addr, cmd_s1_req.addr) // Next cycle in S1
  val cmd_s1_tag = cfg.l2.base.tag(cmd_s1_req.addr)
  val cmd_s1_idx = cfg.l2.base.index(cmd_s1_req.addr)

  // S2 writes are visible here
  val cmd_s1_metadata_readout = for((readout, we) <- metadata.read(cfg.l2.base.index(cmd_pre_s1_addr)).zip(metadata_write_mask.asBools)) yield {
    Mux(RegNext(we && metadata_write_idx === cfg.l2.base.index(cmd_pre_s1_addr)), RegNext(metadata_write_payload), readout)
  }
  val cmd_s1_hitmask = cmd_s1_metadata_readout.map(m => m.valid && m.tag === cfg.l2.base.tag(cmd_s2_req.addr))
  assert(PopCount(cmd_s1_hitmask) <= 1.U)
  val cmd_s1_hit = VecInit(cmd_s1_hitmask).asUInt.orR
  val cmd_s1_metadata = Mux1H(cmd_s1_hitmask, cmd_s1_metadata_readout)

  val cmd_pre_s1_hr_matching_vec = for(m <- mscchrs) yield (
    cfg.l2.base.index(cmd_pre_s1_addr) === m.idx
    && (cfg.l2.base.tag(m.addr_r) === cfg.l2.base.tag(cmd_pre_s1_addr) || cfg.l2.base.tag(m.addr_w) === cfg.l2.base.tag(cmd_pre_s1_addr))
    && m.waiting_r
    // TODO: impl CCHR side
  )
  val cmd_pre_s1_hr_allocting_matched = Bool()
  val cmd_s1_victim_unavail = RegNext(mscchrs.foldLeft(0.U(cfg.l2.base.assoc_cnt.W))((acc, m: L2MSCCHR) => {
    acc | Mux(m.valid && m.idx === cfg.l2.base.index(cmd_pre_s1_addr), UIntToOH(m.assoc, cfg.l2.base.assoc_cnt), 0.U)
  }))
  val cmd_s1_hr_blocked = RegNext(VecInit(cmd_pre_s1_hr_matching_vec).asUInt.orR || cmd_pre_s1_hr_allocting_matched)

  val cmd_s1_victim_prio = RegInit(1.U(cfg.l2.base.assoc_cnt.W))
  cmd_s1_victim_prio := cmd_s1_victim_prio.rotateLeft(1.U)
  val cmd_s1_victim_oh = FirstOneOH(
    ~cmd_s1_victim_unavail,
    cmd_s1_victim_prio
  )
  val cmd_s1_victim = OHToUInt(cmd_s1_victim_oh)
  // If there is no available victim, we are either being flushed or being refilled, block s1
  val cmd_s1_no_avail_victim = cmd_s1_victim_oh.orR

  cmd_s1_s2_int.bits.req := cmd_s1_req
  cmd_s1_s2_int.bits.chosen := cmd_s1_chosen
  cmd_s1_s2_int.bits.hit := cmd_s1_hit
  cmd_s1_s2_int.bits.assoc := Mux(cmd_s1_hit, OHToUInt(cmd_s1_hitmask), cmd_s1_victim)
  cmd_s1_s2_int.bits.metadata := Mux(cmd_s1_hit, cmd_s1_metadata, Mux1H(cmd_s1_victim_oh, cmd_s1_metadata_readout))

  cmd_s1_s2_int.bits.req := cmd_s1_req
  cmd_s1_s2_int.bits.chosen := cmd_s1_chosen
  cmd_s1_s2_int.valid := cmd_s1_valid && !cmd_s1_hr_blocked && !cmd_s1_blocked_by_s2 && !cmd_s1_no_avail_victim

  val cmd_s2_exit = Bool()
  val cmd_s2_valid = RegInit(false.B)
  val cmd_s2_req = Reg(new MemBusCmd(cmd_s1_req.params))
  val cmd_s2_chosen = Reg(UInt(log2Ceil(src.length).W))
  val cmd_s2_hit = Bool()
  val cmd_s2_assoc = UInt(log2Ceil(cfg.l2.base.assoc_cnt).W)
  val cmd_s2_metadata = new L2Metadata

  when(cmd_s2_exit) {
    cmd_s2_valid := cmd_s1_s2_int.fire
    cmd_s2_req := cmd_s1_s2_int.bits.req
    cmd_s2_chosen := cmd_s1_s2_int.bits.chosen
    cmd_s2_hit := cmd_s1_s2_int.bits.hit
    cmd_s2_metadata := cmd_s1_s2_int.bits.metadata
    cmd_s2_assoc := cmd_s1_s2_int.bits.assoc
  }

  val cmd_s2_tag_idx = cfg.l2.base.tag(cmd_s2_req.addr) ## cfg.l2.base.index(cmd_s2_req.addr)
  val cmd_s2_victim_tag_idx = cmd_s2_metadata.tag ## cfg.l2.base.index(cmd_s2_req.addr)

  val cmd_s2_has_victim = cmd_s2_metadata.valid && !cmd_s2_hit

  // In certain cases, even if s2 is exiting, we need to block s1 (to make it read metadata / MSCCHR one more cycle)
  // Block s1 if s2 is accessing exactly the same line
  val cmd_s1_blocked_by_sameline_s2 = cmd_s2_valid && cmd_s2_tag_idx === cmd_s1_tag ## cmd_s1_idx
  // Even if s2 exit, next cycle victim will be in mshr
  val cmd_s1_blocked_by_victim_s2 = cmd_s2_valid && cmd_s2_has_victim && cmd_s2_victim_tag_idx === cmd_s1_tag ## cmd_s1_idx
  cmd_s1_blocked_by_s2 := cmd_s1_blocked_by_sameline_s2 || cmd_s1_blocked_by_victim_s2

  val cmd_s2_self_coherent = cmd_s2_chosen(0)
  val cmd_s2_self_coherence_mask = Mux(cmd_s2_self_coherent, UIntToOH(cmd_s2_chosen >> 1, src.length / 2), 0.U)
  // FIXME: AMO need to invalidate masters
  val cmd_s2_coherence_conflict = (
    cmd_s2_req.op === MemBusOp.read && cmd_s2_metadata.master_dirty
    || cmd_s2_req.op === MemBusOp.occupy && (cmd_s2_metadata.occupation & ~cmd_s2_self_coherence_mask).orR
  )
  // TODO: for right now, coherence conflicts will never happen
  assert(!cmd_s2_coherence_conflict)
  // val cmd_s2_coherence_target: Bits
  // val cmd_s2_coherence_inval: Bool

  // State transfers!
  val cmd_s2_updated_occupation = PriorityMux(Seq(
    // TODO: assert on release, occupation should be exclusive
    (cmd_s2_req.op === MemBusOp.write && cmd_s2_req.subop(MemBusSubOpIdx.RELEASE)) -> 0.U,
    (cmd_s2_req.op === MemBusOp.read) -> (cmd_s2_self_coherence_mask | cmd_s2_metadata.occupation),
    (cmd_s2_req.op === MemBusOp.occupy) -> cmd_s2_self_coherence_mask,
    true.B -> cmd_s2_metadata.occupation,
  ))
  val cmd_s2_updated_master_dirty = PriorityMux(Seq(
    (cmd_s2_req.op === MemBusOp.write && cmd_s2_req.subop(MemBusSubOpIdx.RELEASE)) -> false.B,
    (cmd_s2_req.op === MemBusOp.write && !cmd_s2_req.subop(MemBusSubOpIdx.RELEASE)) -> true.B,
    (cmd_s2_req.op === MemBusOp.occupy) -> true.B,
    (cmd_s2_req.op === MemBusOp.read) -> false.B,
    // TODO: AMO
  ))
  val cmd_s2_updated_dirty = PriorityMux(Seq(
    (cmd_s2_req.op === MemBusOp.occupy) -> true.B,
    true.B -> cmd_s2_metadata.dirty,
  ))
  val cmd_s2_updated_metadata = new L2Metadata
  cmd_s2_updated_metadata.dirty := cmd_s2_updated_dirty
  cmd_s2_updated_metadata.master_dirty := cmd_s2_updated_master_dirty
  cmd_s2_updated_metadata.occupation := cmd_s2_updated_occupation
  cmd_s2_updated_metadata.tag := cfg.l2.base.tag(cmd_s2_req.addr)
  // We can write this one multiple times, so no need to gate it
  metadata.write(
    cfg.l2.base.index(cmd_s2_req.addr),
    VecInit(Seq.fill(cfg.l2.base.assoc_cnt)({ cmd_s2_updated_metadata })),
    mask = UIntToOH(cmd_s2_assoc, cfg.l2.base.assoc_cnt).asBools
  )

  val cmd_s2_hr_allocate = !cmd_s2_hit || cmd_s2_coherence_conflict
  val cmd_s2_hr_allocation_oh = FirstOneOH(mscchrs.map(!_.valid))
  val cmd_s2_hr_free = cmd_s2_hr_allocation_oh.orR

  val cmd_s2_need_coherence = (
    (cmd_s2_req.op === MemBusOp.occupy && cmd_s2_metadata.occupation =/= cmd_s2_self_coherence_mask)
    // TODO: Actually, when reading, occupation should never be the same as self_mask. Need to assert this
    || (cmd_s2_req.op === MemBusOp.read && cmd_s2_metadata.master_dirty && cmd_s2_metadata.occupation =/= cmd_s2_self_coherence_mask)
  )
  val cmd_s2_need_inval = cmd_s2_req.op === MemBusOp.occupy

  // Counting ongoing pending requests
  for(p <- pending_reads) {
    p.matcher := cmd_s2_req.addr
  }
  for(p <- pending_writes) {
    p.matcher := cmd_s2_req.addr
  }
  val cmd_s2_ongoing_bitmask = VecInit(Seq(
    pending_reads.map(_.matched),
    pending_writes.map(_.matched)
  ).flatten).asUInt
  val cmd_s2_ongoing_cnt = PopCount(cmd_s2_ongoing_bitmask)

  for((m, en) <- mscchrs.zip(cmd_s2_hr_allocation_oh.asBools)) {
    when(en && cmd_s2_hr_allocate) {
      m.req_id := cmd_s2_req.id
      m.req_port := cmd_s2_chosen

      m.addr_r := cmd_s2_req.addr
      m.addr_w := (cmd_s2_metadata.tag ## cfg.l2.base.index(cmd_s2_req.addr) ## cfg.l2.base.offset(cmd_s2_req.addr)).asUInt
      m.assoc := cmd_s2_assoc
      assert(m.cnt_i === 0.U)
      assert(m.cnt_r === 0.U)
      assert(m.cnt_w === 0.U)
      assert(!m.pending_i.orR)
      assert(!m.pending_r)
      assert(!m.pending_w)
      assert(!m.waiting_i.orR)
      assert(!m.waiting_r)
      assert(!m.waiting_w)

      m.victim_ongoing_rw := cmd_s2_ongoing_cnt

      val mask_i = Mux(cmd_s2_need_coherence, cmd_s2_metadata.occupation & ~cmd_s2_self_coherence_mask, 0.U)
      m.has_i_data := cmd_s2_need_inval
      m.pending_i := mask_i
      m.waiting_i := mask_i

      m.pending_w := !cmd_s2_hit && cmd_s2_metadata.dirty
      m.waiting_w := !cmd_s2_hit && cmd_s2_metadata.dirty

      m.pending_r := !cmd_s2_hit
      m.waiting_r := !cmd_s2_hit
    }
  }

  // Feed back allocating MSCCHR info
  cmd_pre_s1_hr_allocting_matched := cmd_s2_hr_allocate && (
    cfg.l2.base.index(cmd_pre_s1_addr) === cmd_s2_req.addr
    && (
      cfg.l2.base.tag(cmd_pre_s1_addr) === cfg.l2.base.tag(cmd_s2_req.addr)
      || ((cfg.l2.base.tag(cmd_pre_s1_addr) === cmd_s2_metadata.tag.asUInt) && cmd_s2_metadata.valid)
    )
  )

  ////////////////////
  // Reading (uplink channel)
  ////////////////////
  val read_output_bp = Bool()
  read_output_bp := false.B
  val read_streams = for((s, (h, p)) <- src.zip(pending_read_heads.zip(pending_reads))) yield {
    val stream = DecoupledIO(new BankedMemReq)

    stream.bits.we := false.B
    stream.bits.sbe := -1.S.asUInt
    stream.bits.wdata := DontCare
    require(h.cnt.getWidth == p.pop.bits.subidx.getWidth)
    stream.bits.idx := (p.pop.bits.assoc ## ((h.cnt + p.pop.bits.subidx) + p.pop.bits.idx)).asUInt

    stream.valid := p.pop.valid
    when(stream.fire) {
      h.cnt := h.cnt + 1.U
    }
    p.pop.ready := stream.fire && h.cnt.andR

    stream
  }
  val read_arb = new RRArbiter(new BankedMemReq, read_streams.length)
  (read_arb.io.in, read_streams).zipped.foreach(_ <> _)
  val read_arb_out = read_arb.io.out
  val read_arb_oh = UIntToOH(read_arb.io.chosen)
  val s1_read_arb_fire = RegEnable(read_arb_out.fire, !read_output_bp)
  val s1_read_arb_oh = RegEnable(read_arb_oh, !read_output_bp)
  read_output_bp := s1_read_arb_fire && Mux1H(s1_read_arb_oh, src.map(_.uplink.ready))
  for((uplink, pidx) <- src.map(_.uplink).zipWithIndex) {
    uplink.bits.data := read_resp
    uplink.valid := s1_read_arb_oh(pidx) && s1_read_arb_fire
  }

  for((p, w) <- pending_reads.zip(wakeup_reads)) {
    w.bits := RegNext(p.pop.bits.addr)
    w.valid := RegNext(p.pop.fire)
  }
  ////////////////////
  // downlink channel
  ////////////////////

  // TODO

  for((p, w) <- pending_writes.zip(wakeup_writes)) {
    w.bits := RegNext(p.pop.bits.addr)
    w.valid := RegNext(p.pop.fire)
  }

  ////////////////////
  // resp channel
  ////////////////////

  // TODO

  ////////////////////
  // ack channel
  ////////////////////

  // TODO

  ////////////////////
  // Writeback + Refill
  ////////////////////

  // CMD
  
  val writeMSHRIdxFifo = new Queue(UInt(log2Ceil(mscchrs.length).W), cfg.l2.mscchr_related_fifo_depth)

  // Streams are arranged in (w0, w1, w2, w3, r0, r1, r2, r3),
  // so that we can just trim the selectOH to get a write OH
  val wreq_streams = for(m <- mscchrs) yield {
    val stream = DecoupledIO(new MemBusCmd(cfg.membus_params(L2)))

    stream.bits.id := DontCare
    stream.bits.addr := m.addr_w
    stream.bits.burst := (cfg.l2.base.line_size / 8).U
    stream.bits.size := 3.U // 64 bit
    stream.bits.op := MemBusOp.write

    stream.valid := m.pending_w && m.valid
    when(stream.ready) {
      m.pending_w := false.B
    }

    stream
  }

  val rreq_streams = for((m, idx) <- mscchrs.zipWithIndex) yield {
    val stream = DecoupledIO(new MemBusCmd(cfg.membus_params(L2)))

    stream.bits.id := idx.U
    stream.bits.addr := m.addr_w
    stream.bits.burst := (cfg.l2.base.line_size / 8).U
    stream.bits.size := 3.U // 64 bit
    stream.bits.op := MemBusOp.read

    stream.valid := m.pending_r && m.valid
    when(stream.ready) {
      m.pending_r := false.B
    }
    stream
  }

  val ext_cmd_arb = new Arbiter(new MemBusCmd(cfg.membus_params(L2)), 2 * mscchrs.length)
  (ext_cmd_arb.io.in, Seq(wreq_streams, rreq_streams).flatten).zipped.foreach(_ <> _)
  val w_mshr_idx = ext_cmd_arb.io.chosen
  writeMSHRIdxFifo.io.enq.bits := w_mshr_idx
  writeMSHRIdxFifo.io.enq.valid := w_mshr_idx.orR
  ext_cmd_arb.io.out <> external_bus.cmd
  ext_cmd_arb.io.out.ready := external_bus.cmd.ready && writeMSHRIdxFifo.io.enq.ready
  external_bus.cmd.valid := ext_cmd_arb.io.out.valid && writeMSHRIdxFifo.io.enq.ready

  // Memory width / external bus width
  val transfer_width_ratio = banked_mem_cfg.access_size * 8 / external_bus.params.data_width

  // Writeback reader, 2 stage
  val writeback_s0_send = Bool()
  val writeback_s0_mshr_idx = writeMSHRIdxFifo.io.deq.bits
  val writeback_target_mshr = mscchrs(writeback_s0_mshr_idx)

  // Index is more probable to differ, so we are using assoc ## idx as major idx
  val writeback_s0_idx = (writeback_target_mshr.assoc ## writeback_target_mshr.idx).asUInt
  val writeback_s0_subidx = writeback_target_mshr.addr_w(log2Ceil(cfg.l2.base.line_size), log2Ceil(src(0).params.data_width / 8))

  val writeback_s0_cnt = UInt(log2Ceil(cfg.l2.base.line_size * 8 / external_bus.params.data_width).W)
  val writeback_s0_minor = writeback_s0_cnt(log2Ceil(transfer_width_ratio), 0) // Expand to sbe
  // TODO: do spinal/chisel statically determine these width?
  val writeback_s0_major = writeback_s0_cnt >> log2Ceil(transfer_width_ratio) // Append to subidx

  val writeback_s0_bitmask = UIntToOH(writeback_s0_minor, transfer_width_ratio)
  val writeback_s0_bitmask_expand_ratio = banked_mem_cfg.subbank_cnt / transfer_width_ratio
  assert(writeback_s0_bitmask_expand_ratio > 0) // TODO: implement wbe and lift this
  val writeback_s0_sbe = FillInterleaved(writeback_s0_bitmask_expand_ratio, writeback_s0_bitmask)

  writeback_req.valid := writeback_s0_send
  writeback_req.bits.sbe := writeback_s0_sbe
  writeback_req.bits.idx := (writeback_s0_major + writeback_s0_subidx) + writeback_s0_idx
  writeback_req.bits.we := false.B
  writeback_req.bits.wdata := DontCare

  assert(writeback_target_mshr.waiting_w)
  assert(writeback_target_mshr.cnt_w === writeback_s0_cnt)

  when(writeback_req.fire) {
    writeback_s0_cnt := writeback_s0_cnt + 1.U
    writeback_target_mshr.cnt_w := writeback_s0_cnt + 1.U
    when(writeback_s0_cnt.andR) { // Last transfer
      writeback_target_mshr.waiting_w := false.B
    }
  }

  val writeback_s1_valid = RegInit(false.B)
  val writeback_s1_just_sent = RegNext(writeback_s0_send)
  val writeback_s1_data = writeback_resp.readout.cloneType
  writeback_s1_data := Mux(writeback_s1_just_sent, writeback_resp.readout, RegNext(writeback_s1_data))
  val writeback_s1_bitmask = RegEnable(writeback_s0_bitmask, writeback_s0_send)
  val writeback_s1_muxed = Mux1H(writeback_s1_bitmask, writeback_s1_data.asTypeOf(Vec(transfer_width_ratio, UInt(external_bus.params.data_width.W))))

  external_bus.downlink.valid := writeback_s1_valid
  external_bus.downlink.bits.data := writeback_s1_muxed
  val writeback_s1_exit = external_bus.downlink.ready

  when(writeback_s1_exit) {
    writeback_s1_valid := writeback_req.fire
  }

  val writeback_data_avail = (
    !writeback_target_mshr.has_i_data || writeback_target_mshr.cnt_i =/= writeback_target_mshr.cnt_w || !writeback_target_mshr.pending_i.orR
  )
  writeback_s0_send := writeMSHRIdxFifo.io.deq.valid && (!writeback_s1_valid || writeback_s1_exit) && !writeback_target_mshr.victim_ongoing_rw.orR
  writeMSHRIdxFifo.io.deq.ready := writeback_s0_cnt.andR && (!writeback_s1_valid || writeback_s1_exit)

  /**
    * Refill, 1 stage:
    */
  val refill_id = external_bus.uplink.bits.id
  val refill_target_mshr = mscchrs(refill_id)

  val refill_space_allowed = (
    !refill_target_mshr.waiting_w || refill_target_mshr.cnt_w =/= refill_target_mshr.cnt_r
  )

  val refill_idx = (refill_target_mshr.assoc ## refill_target_mshr.idx).asUInt
  val refill_subidx = refill_target_mshr.addr_w(log2Ceil(cfg.l2.base.line_size), log2Ceil(src(0).params.data_width / 8))

  val refill_cnt = UInt(log2Ceil(cfg.l2.base.line_size * 8 / external_bus.params.data_width).W)
  val refill_minor = refill_cnt(log2Ceil(transfer_width_ratio), 0) // Expand to sbe
  val refill_major = refill_cnt >> log2Ceil(transfer_width_ratio) // Append to subidx

  val refill_bitmask = UIntToOH(refill_minor, transfer_width_ratio)
  val refill_bitmask_expand_ratio = banked_mem_cfg.subbank_cnt / transfer_width_ratio
  assert(refill_bitmask_expand_ratio > 0) // TODO: implement wbe and lift this
  val refill_sbe = FillInterleaved(refill_bitmask_expand_ratio, refill_bitmask)

  // TODO: refill_req.valid timing probably too bad?
  refill_req.valid := refill_space_allowed && external_bus.uplink.valid
  refill_req.bits.sbe := refill_sbe
  refill_req.bits.idx := (refill_major + refill_subidx) + refill_idx
  refill_req.bits.we := true.B
  refill_req.bits.wdata := Fill(transfer_width_ratio, external_bus.uplink.bits.data)

  external_bus.uplink.ready := refill_space_allowed && external_bus.uplink.ready

  assert(!refill_target_mshr.valid)
  assert(refill_target_mshr.cnt_r === refill_cnt)

  when(refill_req.fire) {
    assert(refill_target_mshr.waiting_r)

    refill_cnt := refill_cnt + 1.U
    refill_target_mshr.cnt_w := refill_cnt + 1.U
    when(refill_cnt.andR) { // Last transfer
      assert(!refill_target_mshr.waiting_w)
      writeback_target_mshr.waiting_r := false.B
    }
  }

  ////////////////////
  // MSCCHR Wakeup
  ////////////////////
  for(m <- mscchrs) {
    val matched_wakeups = (wakeup_reads ++ wakeup_writes).map(
      e => e.valid
      && cfg.l2.base.index(m.addr_w) === cfg.l2.base.index(e.bits)
      && cfg.l2.base.tag(m.addr_w) === cfg.l2.base.tag(e.bits)
    )
    val matched_wakeups_cnt = PopCount(matched_wakeups)
    assert(m.victim_ongoing_rw >= matched_wakeups_cnt)
    m.victim_ongoing_rw := m.victim_ongoing_rw - matched_wakeups_cnt
  }

  ////////////////////
  // MSCCHR Finialize + GC
  ////////////////////

  val mscchr_finalize_valid = mscchrs.map(e => e.valid && !e.waiting_i.orR && !e.waiting_r && !e.waiting_w)
  val mscchr_finalize_arb_prio = Reg(1.U(cfg.l2.mscchr_cnt.W))
  mscchr_finalize_arb_prio := mscchr_finalize_arb_prio.rotateLeft(1)
  val mscchr_finalize_arb = FirstOneOH(mscchr_finalize_valid, mscchr_finalize_arb_prio).asBools
  val mscchr_finalize_target = Mux1H(mscchr_finalize_arb, mscchrs)

  mscchr_read_req.valid := VecInit(mscchr_finalize_valid).asUInt.orR
  mscchr_read_req.bits.port_idx := mscchr_finalize_target.req_port
  mscchr_read_req.bits.addr := mscchr_finalize_target.addr_r
  mscchr_read_req.bits.assoc := mscchr_finalize_target.assoc
  mscchr_read_req.bits.idx := mscchr_finalize_target.idx
  mscchr_read_req.bits.subidx := mscchr_finalize_target.subidx

  for((m, arb) <- mscchrs.zip(mscchr_finalize_arb)) {
    when(arb && mscchr_read_req.ready) {
      m.valid := false.B
    }
  }

  ////////////////////
  // pending_read / pending_write scheduler
  ////////////////////
  mscchr_read_req.ready := VecInit(pending_reads.map(_.push.ready))(mscchr_read_req.bits.port_idx)
  direct_read_req.ready := (
      direct_read_req.bits.port_idx =/= mscchr_read_req.bits.port_idx
      || !mscchr_read_req.valid
    ) && VecInit(pending_reads.map(_.push.ready))(direct_read_req.bits.port_idx)
  
  for((r, idx) <- pending_reads.zipWithIndex) {
    r.push.bits := Mux((mscchr_read_req.valid && mscchr_read_req.bits.port_idx === idx.U),
      mscchr_read_req.bits,
      mscchr_read_req.bits,
    )

    r.push.valid := (
      (mscchr_read_req.valid && mscchr_read_req.bits.port_idx === idx.U)
      || (direct_read_req.valid && direct_read_req.bits.port_idx === idx.U)
    )
  }

  direct_write_req.ready := VecInit(pending_writes.map(_.push.ready))(direct_write_req.bits.port_idx)
  for((w, idx) <- pending_writes.zipWithIndex) {
    w.push.bits := direct_write_req.bits
    w.push.valid := direct_write_req.valid && direct_write_req.bits.port_idx === idx.U
  }
}