package meowv64.l2

import spinal.core._
import spinal.lib._
import meowv64.mem._
import meowv64._
import meowv64.config._
import meowv64.util._

/**
  * This is a stub. You can extend it by feeding meow
  */

class L2Cache(implicit cfg: MulticoreConfig) extends Component {
  require(cfg.l2.base.inst_cnt == 1) // TODO: impl banking

  val internal_bus = cfg.cores.map(core => new Bundle {
    val frontend = slave(new MemBus(core.membus_params(Frontend)))
    val backend = slave(new MemBus(core.membus_params(Backend)))
  })
  val external_bus = master(new MemBus(cfg.membus_params(L2)))

  val banks = for(i <- 0 to cfg.l2.base.inst_cnt) yield new L2Inst(i)
  banks(0).external_bus <> external_bus
  for((l, r) <- banks(0).internal_bus.zip(internal_bus)) l <> r
}

class PendingInv extends Bundle {
  val valid = Bool()

  val is_invalidation = Bool() // If not, is 
  val sent = Bool() // If true, already sent to master, but has pending data write

  val addr = UInt(Consts.MAX_PADDR_WIDTH bits)
}

class PendingWrite(implicit cfg: MulticoreConfig) extends Bundle with MatchedData[UInt] {
  val valid = Bool()

  val addr = UInt(Consts.MAX_PADDR_WIDTH bits)
  val assoc = UInt(log2Up(cfg.l2.base.assoc_cnt) bits)
  val idx = UInt(cfg.l2.base.index_width bits)
  val subidx = UInt(log2Up(cfg.l2.base.line_width / cfg.cores(0).membus_params(Frontend).data_width) bits)
  val cnt = UInt(log2Up(cfg.l2.base.line_width / cfg.cores(0).membus_params(Frontend).data_width) bits)

  val associated_inv = UInt(log2Up(cfg.l2.max_pending_inv) bits)

  def matched(matcher: UInt): Bool = cfg.l2.base.tag(addr) === cfg.l2.base.tag(matcher) && cfg.l2.base.index(addr) === cfg.l2.base.index(matcher)
}

class PendingReadHead(implicit cfg: MulticoreConfig) extends Bundle {
  val cnt = UInt(log2Up(cfg.l2.base.line_width / cfg.cores(0).membus_params(Frontend).data_width) bits)
}

class PendingRead(implicit cfg: MulticoreConfig) extends Bundle with MatchedData[UInt] {
  val addr = UInt(Consts.MAX_PADDR_WIDTH bits)

  val assoc = UInt(log2Up(cfg.l2.base.assoc_cnt) bits)
  val idx = UInt(cfg.l2.base.index_width bits)
  val subidx = UInt(log2Up(cfg.l2.base.line_width / cfg.cores(0).membus_params(Frontend).data_width) bits)

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
  val req_port = UInt(log2Up(cfg.cores.length * 2) bits)
  val req_id = UInt(cfg.cores(0).membus_params(Frontend).id_width bits)

  // Master is reading/writing victim. Can send writeback / refill request, but cannot accept data yet, nor cannot invalidate master
  // TODO: how to wakeup?
  val victim_ongoing_rw = UInt(log2Up(cfg.cores.length * (2 * cfg.l2.max_pending_read + cfg.l2.max_pending_write)) bits)

  // TODO: only store one idx##offset for addr_r and addr_w
  val addr_r = UInt(addr_width bits) // Keyword first
  val addr_w = UInt(addr_width bits) // Also keyword first

  // Metadata idx can be fetched from req
  val assoc = UInt(log2Up(cfg.l2.base.assoc_cnt) bits)
  def idx = cfg.l2.base.index(addr_r)
  def subidx = cfg.l2.base.offset(addr_r) >> log2Up(cfg.cores(0).membus_params(Frontend).data_width) // data_width = access_size

  val mask_i = Bits(cfg.cores.length bits)
  
  val has_i_data = Bool()

  val cnt_i = UInt(log2Up(cfg.l2.base.line_size / data_size) bits)
  val cnt_r = UInt(log2Up(cfg.l2.base.line_size / data_size) bits)
  val cnt_w = UInt(log2Up(cfg.l2.base.line_size / data_size) bits)

  // Having to send request to master
  val pending_r = Bool()
  val pending_w = Bool()
  val pending_i = Bits(cfg.cores.length bits)

  // Waiting response from master
  val waiting_w = Bool()
  val waiting_r = Bool()
  val waiting_i = Bits(cfg.cores.length bits)

  // valid \implies (ongoing_rw \implies pending_w)
  // BTW, pending_x is !sent_x && has_x
  assert(!valid || (!victim_ongoing_rw.orR || pending_w))
}

class L2Metadata(implicit val cfg: MulticoreConfig) extends Bundle {
  val valid = Bool()
  val dirty = Bool()
  val occupation = Bits(cfg.cores.length bits)
  val master_dirty = Bool()
  val tag = Bits(cfg.l2.base.tag_width(Consts.MAX_PADDR_WIDTH) bits)
}

class L2Inst(inst_id: Int)(implicit cfg: MulticoreConfig) extends Component {
  val internal_bus = cfg.cores.map(core => new Bundle {
    val frontend = slave(new MemBus(core.membus_params(Frontend)))
    val backend = slave(new MemBus(core.membus_params(Backend)))
  })
  // TODO: assert they are homogeneous
  val external_bus = master(new MemBus(cfg.membus_params(L2)))

  val src = internal_bus.flatMap(core => Seq(core.frontend, core.backend))

  // Memories
  val metadata = Mem(Vec(new L2Metadata, cfg.l2.base.assoc_cnt), cfg.l2.base.line_per_assoc)
  metadata.setName(s"L2.$inst_id.Metadata")

  implicit val banked_mem_cfg = BankedMemConfig(
    total_size = cfg.l2.base.assoc_size,
    access_size = internal_bus(0).frontend.params.data_width / 8,
    concurrency = 4, subbank_cnt = 1,

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
  val writeback_req = Stream(new BankedMemReq)
  val refill_req = Stream(new BankedMemReq)
  val invresp_req = Stream(new BankedMemReq)
  val write_req = Stream(new BankedMemReq)
  val read_req = Stream(new BankedMemReq)

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
      Some(new MatchingQueue(new PendingWrite, UInt(Consts.MAX_PADDR_WIDTH bits), cfg.l2.max_pending_write))
    else None
  ))
  // Unfinished invalidates must corresponds to MSCCHR, so there is no need to match / wakeup other MSCCHRs
  val pending_invs = src.flatMap(bus =>
    if(bus.params.bus_type.with_coherence)
      Some(new StreamFifo(new PendingInv, cfg.l2.max_pending_inv))
    else
      None
  )
  val pending_read_heads = src.map(_ => Reg(new PendingReadHead))
  val pending_reads = src.map(_ => new MatchingQueue(new PendingRead, UInt(Consts.MAX_PADDR_WIDTH bits), cfg.l2.max_pending_read))
  require(pending_writes.length == cfg.cores.length)

  // Wakeups from read/write port
  val wakeup_reads = src.map(_ => Flow(UInt(Consts.MAX_PADDR_WIDTH bits)))
  val wakeup_writes = src.flatMap(e => if(e.params.bus_type.with_write) Some(Flow(UInt(Consts.MAX_PADDR_WIDTH bits))) else None)

  class TargetedPendingRead extends PendingRead {
    val port_idx = UInt(log2Up(src.length) bits)
  }
  class TargetedPendingWrite extends PendingWrite {
    val port_idx = UInt(log2Up(src.length) bits)
  }

  val direct_read_req = Stream(new TargetedPendingRead)
  val mshr_read_req = Stream(new TargetedPendingRead)

  val direct_writre_req = Stream(new TargetedPendingWrite)
  val mshr_writre_req = Stream(new TargetedPendingWrite)
  // FIXME: Schedule reads / writes

  // TODO: queue
  val cmd_arb = new StreamArbiter(new MemBusCmd(src(0).params), src.length)(StreamArbiter.Arbitration.roundRobin, StreamArbiter.Lock.none)
  (cmd_arb.io.inputs, src).zipped.foreach(_ <> _.cmd)
  val downlink_arb = StreamArbiterFactory.roundRobin.noLock.on(src.map(_.downlink).filter(_ != null)) // core_cnt sources
  val resp_arb = StreamArbiterFactory.roundRobin.noLock.on(src.map(_.resp).filter(_ != null)) // core_cnt sources
  // Ack channel doesn't need to be arbitered

  // MSHRs
  val mscchrs = Reg(Vec(new L2MSCCHR, cfg.l2.mscchr_cnt))
  for(m <- mscchrs) {
    m.valid init(False)
    m.cnt_i init(0)
    m.cnt_w init(0)
    m.cnt_r init(0)
  }

  ////////////////////
  // cmd channel
  ////////////////////

  /**
    * CMD channel messages are processed in 3 stages
    * - s0: Read MSCCHR and metadata.
    * - s1: Block by MSCCHR(done_r = False). Also block if the previous request is at the same line. Re-read MSCCHR / metadata if blocked.
    *   Tag comparision, hit detection. Metadata / MSCCHR allocation from s2 forwarded. Victim selection happens here.
    * - s2: Edit directory. If miss / coherence conflict, send to MSCCHR. Otherwise, send directly to r/w buffer.
    */
  
  // s0 is essentially a no-op
  // int stands for interface
  val cmd_s0_s1_int = Stream(new Bundle {
    val req = new MemBusCmd(cmd_arb.io.output.payload.params)
    val chosen = UInt(log2Up(src.length) bits)
  })
  cmd_s0_s1_int <> cmd_arb.io.output ~~ {r => new Bundle {
    val req = r
    val chosen = cmd_arb.io.chosen
  }}

  val cmd_s1_s2_int = Stream(new Bundle {
    val req = new MemBusCmd(cmd_arb.io.output.payload.params)
    val chosen = UInt(log2Up(src.length) bits)
    val hit = Bool()
    val assoc = UInt(log2Up(cfg.l2.base.assoc_cnt) bits)
    // Metadata of the hit assoc, or the selected victim
    val metadata = new L2Metadata
  })

  val cmd_s1_blocked_by_s2 = Bool()

  val cmd_s1_valid = RegInit(False)
  val cmd_s1_req = Reg(new MemBusCmd(cmd_arb.io.output.payload.params))
  val cmd_s1_chosen = Reg(UInt(log2Up(src.length) bits))
  cmd_s0_s1_int.ready := !cmd_s1_valid || cmd_s1_s2_int.ready

  when(cmd_s1_s2_int.fire) {
    cmd_s1_valid := cmd_s0_s1_int.fire
    cmd_s1_req := cmd_s0_s1_int.payload.req
    cmd_s1_chosen := cmd_s0_s1_int.payload.chosen
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

  val cmd_pre_s1_addr = Mux(cmd_s0_s1_int.fire, cmd_s0_s1_int.payload.req.addr, cmd_s1_req.addr) // Next cycle in S1
  val cmd_s1_tag = cfg.l2.base.tag(cmd_s1_req.addr)
  val cmd_s1_idx = cfg.l2.base.index(cmd_s1_req.addr)

  // FIXME: make sure S3 write is visible here
  val cmd_s1_metadata_readout = metadata.readSync(cfg.l2.base.index(cmd_pre_s1_addr))
  val cmd_s1_hitmask = cmd_s1_metadata_readout.map(m => m.valid && m.tag === cfg.l2.base.tag(cmd_s2_req.addr).asBits)
  assert(CountOne(cmd_s1_hitmask) <= 1)
  val cmd_s1_hit = Vec(cmd_s1_hitmask).orR
  val cmd_s1_metadata = MuxOH(cmd_s1_hitmask, cmd_s1_metadata_readout)

  // FIXME: mux in mshrcc allocation
  val cmd_s1_hr_matching_vec = for(m <- mscchrs) yield (
    cfg.l2.base.index(cmd_pre_s1_addr) === m.idx
    && cfg.l2.base.tag(m.addr_r) === cfg.l2.base.tag(cmd_pre_s1_addr)
    && m.waiting_r
    // TODO: impl CCHR side
  )
  val cmd_s1_victim_unavail = RegNext(mscchrs.foldLeft(B(0, cfg.l2.base.assoc_cnt bits))((acc, m: L2MSCCHR) => {
    acc | Mux(m.valid && m.idx === cfg.l2.base.index(cmd_pre_s1_addr), UIntToOh(m.assoc, cfg.l2.base.assoc_cnt), B(0))
  }))
  val cmd_s1_hr_blocked = RegNext(Vec(cmd_s1_hr_matching_vec).orR)

  // TODO: select victim and avoid any ongoing same-idx MSHR
  val cmd_s1_victim_prio = RegInit(B(1, cfg.l2.base.assoc_cnt bits))
  cmd_s1_victim_prio := cmd_s1_victim_prio(0) ## cmd_s1_victim_prio >> 1
  val cmd_s1_victim_oh = OHMasking.roundRobin(
    ~cmd_s1_victim_unavail,
    cmd_s1_victim_prio
  )
  val cmd_s1_victim = OHToUInt(cmd_s1_victim_oh)
  // If there is no available victim, we are either being flushed or being refilled, block s1
  val cmd_s1_no_avail_victim = cmd_s1_victim_oh.orR

  cmd_s1_s2_int.payload.req := cmd_s1_req
  cmd_s1_s2_int.payload.chosen := cmd_s1_chosen
  cmd_s1_s2_int.payload.hit := cmd_s1_hit
  cmd_s1_s2_int.payload.assoc := Mux(cmd_s1_hit, OHToUInt(cmd_s1_hitmask), cmd_s1_victim)
  cmd_s1_s2_int.payload.metadata := Mux(cmd_s1_hit, cmd_s1_metadata, MuxOH(cmd_s1_victim_oh, cmd_s1_metadata_readout))

  cmd_s1_s2_int.payload.req := cmd_s1_req
  cmd_s1_s2_int.payload.chosen := cmd_s1_chosen
  cmd_s1_s2_int.valid := cmd_s1_valid && !cmd_s1_hr_blocked && !cmd_s1_blocked_by_s2 && !cmd_s1_no_avail_victim

  val cmd_s2_exit = Bool()
  val cmd_s2_valid = RegInit(False)
  val cmd_s2_req = Reg(new MemBusCmd(cmd_s1_req.params))
  val cmd_s2_chosen = Reg(UInt(log2Up(src.length) bits))
  val cmd_s2_hit = Bool()
  val cmd_s2_assoc = UInt(log2Up(cfg.l2.base.assoc_cnt) bits)
  val cmd_s2_metadata = new L2Metadata

  when(cmd_s2_exit) {
    cmd_s2_valid := cmd_s1_s2_int.fire
    cmd_s2_req := cmd_s1_s2_int.payload.req
    cmd_s2_chosen := cmd_s1_s2_int.payload.chosen
    cmd_s2_hit := cmd_s1_s2_int.payload.hit
    cmd_s2_metadata := cmd_s1_s2_int.payload.metadata
    cmd_s2_assoc := cmd_s1_s2_int.payload.assoc
  }

  val cmd_s2_tag_idx = cfg.l2.base.tag(cmd_s2_req.addr) ## cfg.l2.base.index(cmd_s2_req.addr)
  val cmd_s2_victim_tag_idx = cmd_s2_metadata.tag ## cfg.l2.base.index(cmd_s2_req.addr)

  val cmd_s2_has_victim = cmd_s2_metadata.valid && !cmd_s2_hit
  // Block s1 if s2 is accessing exactly the same line
  // TODO: is this actually needed? if !cmd_s2_exit, s1 should never be able to move?
  val cmd_s1_blocked_by_sameline_s2 = cmd_s2_valid && !cmd_s2_exit && cmd_s2_tag_idx === cmd_s1_tag ## cmd_s1_idx
  // Even if s2 exit, next cycle victim will be in mshr, so no !cmd_s2_exit here
  val cmd_s1_blocked_by_victim_s2 = cmd_s2_valid && cmd_s2_has_victim && cmd_s2_victim_tag_idx === cmd_s1_tag ## cmd_s1_idx
  cmd_s1_blocked_by_s2 := cmd_s1_blocked_by_sameline_s2 || cmd_s1_blocked_by_victim_s2

  val cmd_s2_self_coherent = cmd_s2_chosen(0)
  val cmd_s2_self_coherence_mask = Mux(cmd_s2_self_coherent, UIntToOh(cmd_s2_chosen >> 1, src.length / 2), B(0))
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
    (cmd_s2_req.op === MemBusOp.write && cmd_s2_req.subop(MemBusSubOpIdx.RELEASE)) -> B(0),
    (cmd_s2_req.op === MemBusOp.read) -> (cmd_s2_self_coherence_mask | cmd_s2_metadata.occupation),
    (cmd_s2_req.op === MemBusOp.occupy) -> cmd_s2_self_coherence_mask,
    True -> cmd_s2_metadata.occupation,
  ))
  val cmd_s2_updated_master_dirty = PriorityMux(Seq(
    (cmd_s2_req.op === MemBusOp.write && cmd_s2_req.subop(MemBusSubOpIdx.RELEASE)) -> False,
    (cmd_s2_req.op === MemBusOp.write && !cmd_s2_req.subop(MemBusSubOpIdx.RELEASE)) -> True,
    (cmd_s2_req.op === MemBusOp.occupy) -> True,
    (cmd_s2_req.op === MemBusOp.read) -> False,
    // TODO: AMO
  ))
  val cmd_s2_updated_dirty = PriorityMux(Seq(
    (cmd_s2_req.op === MemBusOp.occupy) -> True,
    True -> cmd_s2_metadata.dirty,
  ))
  val cmd_s2_updated_metadata = new L2Metadata
  cmd_s2_updated_metadata.dirty := cmd_s2_updated_dirty
  cmd_s2_updated_metadata.master_dirty := cmd_s2_updated_master_dirty
  cmd_s2_updated_metadata.occupation := cmd_s2_updated_occupation
  cmd_s2_updated_metadata.tag := cfg.l2.base.tag(cmd_s2_req.addr).asBits
  // We can write this one multiple times, so no need to gate it
  metadata.write(
    cfg.l2.base.index(cmd_s2_req.addr),
    Vec(Seq.fill(cfg.l2.base.assoc_cnt)({ cmd_s2_updated_metadata })),
    mask = UIntToOh(cmd_s2_assoc, cfg.l2.base.assoc_cnt)
  )

  val cmd_s2_hr_allocate = !cmd_s2_hit || cmd_s2_coherence_conflict
  val cmd_s2_hr_allocation_oh = OHMasking.first(mscchrs.map(!_.valid))
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
  val cmd_s2_ongoing_bitmask = Seq(
    pending_reads.map(_.matched),
    pending_writes.map(_.matched)
  ).flatten.seq.asBits
  val cmd_s2_ongoing_cnt = CountOne(cmd_s2_ongoing_bitmask)

  for((m, en) <- mscchrs.zip(cmd_s2_hr_allocation_oh)) {
    when(en && cmd_s2_hr_allocate) {
      m.addr_r := cmd_s2_req.addr
      m.addr_w := (cmd_s2_metadata.tag ## cfg.l2.base.index(cmd_s2_req.addr) ## cfg.l2.base.offset(cmd_s2_req.addr)).asUInt
      m.assoc := cmd_s2_assoc
      assert(m.cnt_i === 0)
      assert(m.cnt_r === 0)
      assert(m.cnt_w === 0)
      assert(!m.pending_i.orR)
      assert(!m.pending_r)
      assert(!m.pending_w)
      assert(!m.waiting_i.orR)
      assert(!m.waiting_r)
      assert(!m.waiting_w)

      m.victim_ongoing_rw := cmd_s2_ongoing_cnt

      val mask_i = Mux(cmd_s2_need_coherence, cmd_s2_metadata.occupation & ~cmd_s2_self_coherence_mask, B(0))
      m.has_i_data := cmd_s2_need_inval
      m.pending_i := mask_i
      m.waiting_i := mask_i

      m.pending_w := !cmd_s2_hit && cmd_s2_metadata.dirty
      m.waiting_w := !cmd_s2_hit && cmd_s2_metadata.dirty

      m.pending_r := !cmd_s2_hit
      m.waiting_r := !cmd_s2_hit
    }
  }

  ////////////////////
  // Reading (uplink channel)
  ////////////////////
  val read_output_bp = Bool()
  read_output_bp := False
  val read_streams = for((s, (h, p)) <- src.zip(pending_read_heads.zip(pending_reads))) yield {
    val stream = Stream(new BankedMemReq)

    stream.payload.we := False
    stream.payload.sbe := B(-1)
    stream.payload.wdata.assignDontCare()
    require(h.cnt.getWidth == p.pop.payload.subidx.getWidth)
    stream.idx := (p.pop.payload.assoc ## ((h.cnt + p.pop.payload.subidx) + p.pop.payload.idx)).as(UInt())

    stream.valid := p.pop.valid
    when(stream.fire) {
      h.cnt := h.cnt + 1
    }
    p.pop.ready := stream.fire && h.cnt.andR

    stream
  }
  val read_arb = new StreamArbiter(new BankedMemReq, read_streams.length)(StreamArbiter.Arbitration.roundRobin, StreamArbiter.Lock.none)
  (read_arb.io.inputs, read_streams).zipped.foreach(_ << _)
  val read_arb_out = read_arb.io.output
  val read_arb_oh = read_arb.io.chosenOH
  val s1_read_arb_fire = RegNextWhen(read_arb_out.fire, !read_output_bp)
  val s1_read_arb_oh = RegNextWhen(read_arb_oh, !read_output_bp)
  read_output_bp := s1_read_arb_fire && MuxOH(s1_read_arb_oh, src.map(_.uplink.ready))
  for((uplink, pidx) <- src.map(_.uplink).zipWithIndex) {
    uplink.payload.data := read_resp.asBits
    uplink.valid := s1_read_arb_oh(pidx) && s1_read_arb_fire
  }

  for((p, w) <- pending_reads.zip(wakeup_reads)) {
    w.payload := RegNext(p.pop.payload.addr)
    w.valid := RegNext(p.pop.fire)
  }
  ////////////////////
  // downlink channel
  ////////////////////

  // TODO

  for((p, w) <- pending_writes.zip(wakeup_writes)) {
    w.payload := RegNext(p.pop.payload.addr)
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
  
  val writeMSHRIdxFifo = StreamFifo(UInt(log2Up(mscchrs.length) bits), cfg.l2.mscchr_related_fifo_depth)

  // Streams are arranged in (w0, w1, w2, w3, r0, r1, r2, r3),
  // so that we can just trim the selectOH to get a write OH
  val wreq_streams = for(m <- mscchrs) yield {
    val stream = Stream(new MemBusCmd(cfg.membus_params(L2)))

    stream.payload.id.assignDontCare()
    stream.payload.addr := m.addr_w
    stream.payload.burst := cfg.l2.base.line_width / 64
    stream.payload.size := 3 // 64 bit
    stream.payload.op := MemBusOp.write

    stream.valid := m.pending_w && m.valid
    when(stream.ready) {
      m.pending_w := False
    }

    stream
  }

  val rreq_streams = for((m, idx) <- mscchrs.zipWithIndex) yield {
    val stream = Stream(new MemBusCmd(cfg.membus_params(L2)))

    stream.payload.id := idx
    stream.payload.addr := m.addr_w
    stream.payload.burst := cfg.l2.base.line_width / 64
    stream.payload.size := 3 // 64 bit
    stream.payload.op := MemBusOp.read

    stream.valid := m.pending_r && m.valid
    when(stream.ready) {
      m.pending_r := False
    }
    stream
  }

  val ext_cmd_arb = new StreamArbiter(new MemBusCmd(cfg.membus_params(L2)), 2 * mscchrs.length)(StreamArbiter.Arbitration.lowerFirst, StreamArbiter.Lock.transactionLock)
  (ext_cmd_arb.io.inputs, Seq(wreq_streams, rreq_streams).flatten).zipped.foreach(_ <> _)
  val w_mshr_idx = ext_cmd_arb.io.chosen
  writeMSHRIdxFifo.io.push.payload := w_mshr_idx
  writeMSHRIdxFifo.io.push.valid := w_mshr_idx.orR
  ext_cmd_arb.io.output <> external_bus.cmd
  ext_cmd_arb.io.output.ready := external_bus.cmd.ready && writeMSHRIdxFifo.io.push.ready
  external_bus.cmd.valid := ext_cmd_arb.io.output.valid && writeMSHRIdxFifo.io.push.ready

  // Memory width / external bus width
  val transfer_width_ratio = banked_mem_cfg.access_size * 8 / external_bus.params.data_width

  // Writeback reader, 2 stage
  val writeback_s0_send = Bool()
  val writeback_s0_mshr_idx = writeMSHRIdxFifo.io.pop.payload
  val writeback_target_mshr = mscchrs(writeback_s0_mshr_idx)

  // Index is more probable to differ, so we are using assoc ## idx as major idx
  val writeback_s0_idx = (writeback_target_mshr.assoc ## writeback_target_mshr.idx).as(UInt())
  val writeback_s0_subidx = writeback_target_mshr.addr_w(log2Up(cfg.l2.base.line_size) downto log2Up(src(0).params.data_width / 8))

  val writeback_s0_cnt = UInt(log2Up(cfg.l2.base.line_size * 8 / external_bus.params.data_width) bits)
  val writeback_s0_minor = writeback_s0_cnt(0, log2Up(transfer_width_ratio) bits) // Expand to sbe
  // TODO: do spinal/chisel statically determine these width?
  val writeback_s0_major = writeback_s0_cnt >> log2Up(transfer_width_ratio) // Append to subidx

  val writeback_s0_bitmask = UIntToOh(writeback_s0_minor, transfer_width_ratio)
  val writeback_s0_bitmask_expand_ratio = banked_mem_cfg.subbank_cnt / transfer_width_ratio
  assert(writeback_s0_bitmask_expand_ratio > 0) // TODO: implement wbe and lift this
  val writeback_s0_sbe = ExpandInterleave(writeback_s0_bitmask, writeback_s0_bitmask_expand_ratio)

  writeback_req.valid := writeback_s0_send
  writeback_req.payload.sbe := writeback_s0_sbe
  writeback_req.payload.idx := (writeback_s0_major + writeback_s0_subidx) + writeback_s0_idx
  writeback_req.payload.we := False
  writeback_req.payload.wdata.assignDontCare()

  assert(writeback_target_mshr.waiting_w)
  assert(writeback_target_mshr.cnt_w === writeback_s0_cnt)

  when(writeback_req.fire) {
    writeback_s0_cnt := writeback_s0_cnt + 1
    writeback_target_mshr.cnt_w := writeback_s0_cnt + 1
    when(writeback_s0_cnt.andR) { // Last transfer
      writeback_target_mshr.waiting_w := False
    }
  }

  val writeback_s1_valid = RegInit(False)
  val writeback_s1_just_sent = RegNext(writeback_s0_send)
  val writeback_s1_data = writeback_resp.readout.clone()
  writeback_s1_data := Mux(writeback_s1_just_sent, writeback_resp.readout, RegNext(writeback_s1_data))
  val writeback_s1_bitmask = RegNextWhen(writeback_s0_bitmask, writeback_s0_send)
  val writeback_s1_muxed = MuxOH(writeback_s1_bitmask, writeback_s1_data.as(Vec(Bits(external_bus.params.data_width bits), transfer_width_ratio)))

  external_bus.downlink.valid := writeback_s1_valid
  external_bus.downlink.payload.data := writeback_s1_muxed
  val writeback_s1_exit = external_bus.downlink.ready

  when(writeback_s1_exit) {
    writeback_s1_valid := writeback_req.fire
  }

  val writeback_data_avail = (
    !writeback_target_mshr.has_i_data || writeback_target_mshr.cnt_i =/= writeback_target_mshr.cnt_w || !writeback_target_mshr.pending_i.orR
  )
  writeback_s0_send := writeMSHRIdxFifo.io.pop.valid && (!writeback_s1_valid || writeback_s1_exit) && !writeback_target_mshr.victim_ongoing_rw.orR
  writeMSHRIdxFifo.io.pop.ready := writeback_s0_cnt.andR && (!writeback_s1_valid || writeback_s1_exit)

  /**
    * Refill, 1 stage:
    */
  val refill_id = external_bus.uplink.payload.id
  val refill_target_mshr = mscchrs(refill_id)

  val refill_space_allowed = (
    !refill_target_mshr.waiting_w || refill_target_mshr.cnt_w =/= refill_target_mshr.cnt_r
  )

  val refill_idx = (refill_target_mshr.assoc ## refill_target_mshr.idx).as(UInt())
  val refill_subidx = refill_target_mshr.addr_w(log2Up(cfg.l2.base.line_size) downto log2Up(src(0).params.data_width / 8))

  val refill_cnt = UInt(log2Up(cfg.l2.base.line_size * 8 / external_bus.params.data_width) bits)
  val refill_minor = refill_cnt(0, log2Up(transfer_width_ratio) bits) // Expand to sbe
  val refill_major = refill_cnt >> log2Up(transfer_width_ratio) // Append to subidx

  val refill_bitmask = UIntToOh(refill_minor, transfer_width_ratio)
  val refill_bitmask_expand_ratio = banked_mem_cfg.subbank_cnt / transfer_width_ratio
  assert(refill_bitmask_expand_ratio > 0) // TODO: implement wbe and lift this
  val refill_sbe = ExpandInterleave(refill_bitmask, refill_bitmask_expand_ratio)

  // TODO: refill_req.valid timing probably too bad?
  refill_req.valid := refill_space_allowed && external_bus.uplink.valid
  refill_req.payload.sbe := refill_sbe
  refill_req.payload.idx := (refill_major + refill_subidx) + refill_idx
  refill_req.payload.we := True
  refill_req.payload.wdata := Duplicate(external_bus.uplink.payload.data, transfer_width_ratio)

  // FIXME: move to GC
  // refill_pending_read_fifo.io.push.valid := refill_req.fire && refill_cnt.andR
  // refill_pending_read_fifo.io.push.payload.port_idx := refill_id
  // refill_pending_read_fifo.io.push.payload.addr := refill_target_mshr.addr_r
  // refill_pending_read_fifo.io.push.payload.assoc := refill_target_mshr.assoc
  // refill_pending_read_fifo.io.push.payload.idx := refill_target_mshr.idx
  // refill_pending_read_fifo.io.push.payload.subidx := refill_subidx

  external_bus.uplink.ready := refill_space_allowed && external_bus.uplink.ready

  assert(!refill_target_mshr.valid)
  assert(refill_target_mshr.cnt_r === refill_cnt)

  when(refill_req.fire) {
    assert(refill_target_mshr.waiting_r)

    refill_cnt := refill_cnt + 1
    refill_target_mshr.cnt_w := refill_cnt + 1
    when(refill_cnt.andR) { // Last transfer
      assert(!refill_target_mshr.waiting_w)
      writeback_target_mshr.waiting_r := False
    }
  }

  ////////////////////
  // MSCCHR Wakeup
  ////////////////////
  for(m <- mscchrs) {
    val matched_wakeups = (wakeup_reads ++ wakeup_writes).map(
      e => e.valid
      && cfg.l2.base.index(m.addr_w) === cfg.l2.base.index(e.payload)
      && cfg.l2.base.tag(m.addr_w) === cfg.l2.base.tag(e.payload)
    )
    val matched_wakeups_cnt = CountOne(matched_wakeups)
    assert(m.victim_ongoing_rw >= matched_wakeups_cnt)
    m.victim_ongoing_rw := m.victim_ongoing_rw - matched_wakeups_cnt
  }

  ////////////////////
  // MSCCHR Finialize + GC
  ////////////////////

  val mscchr_finialize_valid = mscchrs.map(e => !e.waiting_i.orR && !e.waiting_r && !e.waiting_w)
  // FIXME: impl
}