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

class PendingInv extends Bundle with MatchedData[UInt] {
  val valid = Bool()

  val is_invalidation = Bool() // If not, is 
  val sent = Bool() // If true, already sent to master, but has pending data write

  val addr = UInt(Consts.MAX_PADDR_WIDTH bits)

  def matched(matcher: UInt): Bool = addr === matcher
}

class PendingWrite(implicit cfg: MulticoreConfig) extends Bundle {
  val valid = Bool()

  val addr = UInt(Consts.MAX_PADDR_WIDTH bits)
  val idx = UInt(cfg.l2.base.index_width bits)
  val subidx = UInt(log2Up(cfg.l2.base.line_width / cfg.cores(0).membus_params(Frontend).data_width) bits)
  val cnt = UInt(log2Up(cfg.l2.base.line_width / cfg.cores(0).membus_params(Frontend).data_width) bits)

  val associated_inv = UInt(log2Up(cfg.l2.max_pending_inv) bits)
}

class PendingReadHead(implicit cfg: MulticoreConfig) extends Bundle {
  val cnt = UInt(log2Up(cfg.l2.base.line_width / cfg.cores(0).membus_params(Frontend).data_width) bits)
}

class PendingRead(implicit cfg: MulticoreConfig) extends Bundle with MatchedData[UInt] {
  val addr = UInt(Consts.MAX_PADDR_WIDTH bits)
  // Memory index, with assoc
  val idx = UInt(cfg.l2.base.index_width bits)
  val subidx = UInt(log2Up(cfg.l2.base.line_width / cfg.cores(0).membus_params(Frontend).data_width) bits)

  def matched(matcher: UInt): Bool = addr === matcher
}

class L2MSHR(implicit cfg: MulticoreConfig) extends Bundle {
  val data_size = cfg.membus_params(L2).data_width / 8
  val addr_width = cfg.membus_params(L2).addr_width

  val valid = Bool()

  // For replying
  val req = new MemBusCmd(cfg.cores(0).membus_params(Backend))

  // Metadata idx can be fetched from req
  val victim_oh = Bits(cfg.l2.base.assoc_cnt bits)

  val idx = UInt(cfg.l2.base.index_width bits)
  val subidx = UInt(log2Up(cfg.l2.base.line_width / cfg.cores(0).membus_params(Frontend).data_width) bits)

  val addr_r = UInt(addr_width bits) // Keyword first
  val addr_w = UInt(addr_width bits) // Also keyword first

  val cnt_r = UInt(log2Up(cfg.l2.base.line_size / data_size) bits)
  val cnt_w = UInt(log2Up(cfg.l2.base.line_size / data_size) bits)

  val has_w = Bool()

  val sent_r = Bool()
  val sent_w = Bool()

  val done_w = Bool()
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
  val valids = Mem(Vec(Bool(), cfg.l2.base.assoc_cnt), cfg.l2.base.line_per_assoc)
  val dirtys = Mem(Vec(Bool(), cfg.l2.base.assoc_cnt), cfg.l2.base.line_per_assoc)
  val occupations = Mem(Vec(Bits(cfg.cores.length bits), cfg.l2.base.assoc_cnt), cfg.l2.base.line_per_assoc)
  val master_dirtys = Mem(Vec(Bool(), cfg.l2.base.assoc_cnt), cfg.l2.base.line_per_assoc) // Master is dirty
  val tags = Mem(
    Vec(UInt(cfg.l2.base.tag_width(Consts.MAX_PADDR_WIDTH) bits), cfg.l2.base.assoc_cnt), cfg.l2.base.line_per_assoc)

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
  val data = new BankedMem(s"L2 Data")

  // Pendings
  val pending_writes = src.flatMap(bus => if(bus.params.bus_type.with_write) Some(Reg(new PendingWrite)) else None)
  val pending_invs = src.flatMap(bus =>
    if(bus.params.bus_type.with_coherence)
      Some(new MatchingQueue(new PendingInv, UInt(Consts.MAX_PADDR_WIDTH bits), cfg.l2.max_pending_inv))
    else
      None
  )
  val pending_read_heads = src.map(_ => Reg(new PendingReadHead))
  val pending_reads = src.map(_ => new MatchingQueue(new PendingRead, UInt(Consts.MAX_PADDR_WIDTH bits), cfg.l2.max_pending_read))
  require(pending_writes.length == cfg.cores.length)

  valids.setName(s"L2Inst $inst_id / valids")
  dirtys.setName(s"L2Inst $inst_id / valids")
  occupations.setName(s"L2Inst $inst_id / occupations")
  master_dirtys.setName(s"L2Inst $inst_id / valids")
  tags.setName(s"L2Inst $inst_id / tags")

  // TODO: queue
  val cmd_arb = StreamArbiterFactory.roundRobin.noLock.on(src.map(_.cmd)) // 2 * core_cnt sources, idx >> 1 = core src
  val downlink_arb = StreamArbiterFactory.roundRobin.noLock.on(src.map(_.downlink).filter(_ != null)) // core_cnt sources
  val resp_arb = StreamArbiterFactory.roundRobin.noLock.on(src.map(_.resp).filter(_ != null)) // core_cnt sources
  // Ack channel doesn't need to be arbitered

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

  // MSHRs
  val mshrs = Reg(Vec(new L2MSHR, cfg.l2.mshr_cnt))
  for(m <- mshrs) {
    m.valid init(False)
  }

  ////////////////////
  // cmd channel
  ////////////////////

  // TODO

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
    stream.idx := (h.cnt + p.pop.payload.subidx) + p.pop.payload.idx

    stream.valid := p.pop.valid
    when(stream.fire) {
      h.cnt := h.cnt + 1
    }
    p.pop.ready := stream.fire && h.cnt.andR

    stream
  }
  val read_arb = new StreamArbiter(new BankedMemReq, read_streams.length)(StreamArbiter.Arbitration.roundRobin, StreamArbiter.Lock.transactionLock)
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

  ////////////////////
  // downlink channel
  ////////////////////

  // TODO

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
  
  val writeMSHRIdxFifo = StreamFifo(UInt(log2Up(mshrs.length) bits), mshrs.length)

  // Streams are arranged in (w0, w1, w2, w3, r0, r1, r2, r3),
  // so that we can just trim the selectOH to get a write OH
  val wreq_streams = for(m <- mshrs) yield {
    val stream = Stream(new MemBusCmd(cfg.membus_params(L2)))

    stream.payload.id.assignDontCare()
    stream.payload.addr := m.addr_w
    stream.payload.burst := cfg.l2.base.line_width / 64
    stream.payload.size := 3 // 64 bit
    stream.payload.op := MemBusOp.write

    stream.valid := !m.sent_w && m.has_w && m.valid
    when(stream.ready) {
      m.sent_w := True
    }

    stream
  }

  val rreq_streams = for((m, idx) <- mshrs.zipWithIndex) yield {
    val stream = Stream(new MemBusCmd(cfg.membus_params(L2)))

    stream.payload.id := idx
    stream.payload.addr := m.addr_w
    stream.payload.op := MemBusOp.read
    stream.payload.size := 3 // 64 bit
    stream.payload.op := MemBusOp.write

    stream.valid := !m.sent_r && m.valid
    when(stream.ready) {
      m.sent_r := True
    }
    stream
  }

  val ext_cmd_arb = new StreamArbiter(new MemBusCmd(cfg.membus_params(L2)), 2 * mshrs.length)(StreamArbiter.Arbitration.lowerFirst, StreamArbiter.Lock.transactionLock)
  (ext_cmd_arb.io.inputs, Seq(wreq_streams, rreq_streams).flatten).zipped.foreach(_ <> _)
  val w_mshr_idx = ext_cmd_arb.io.chosen(0, mshrs.length bits)
  writeMSHRIdxFifo.io.push.payload := w_mshr_idx
  writeMSHRIdxFifo.io.push.valid := w_mshr_idx.orR
  when(writeMSHRIdxFifo.io.push.valid) {
    assert(writeMSHRIdxFifo.io.push.fire)
  }
  ext_cmd_arb.io.output <> external_bus.cmd

  // Memory width / external bus width
  val transfer_width_ratio = banked_mem_cfg.access_size * 8 / external_bus.params.data_width

  val read_transfer_bitmask = B(1, transfer_width_ratio bits)

  // Writeback reader, 2 stage
  val s0_writeback_send = Bool()
  val s0_writeback_mshr = writeMSHRIdxFifo.io.pop.payload
  val s0_writeback_cnt = UInt(log2Up(cfg.l2.base.line_size * 8 / external_bus.params.data_width) bits)
  val s0_writeback_minor = s0_writeback_cnt(0, log2Up(transfer_width_ratio) bits)
  val s0_writeback_major = s0_writeback_cnt >> log2Up(transfer_width_ratio) // FIXME: do spinal/chisel statically determine these width?
  val s0_writeback_bitmask = UIntToOh(s0_writeback_minor, transfer_width_ratio)

  writeback_req.valid := s0_writeback_send
  // FIXME: finish writeback_req

  val writeback_target_mshr = mshrs(s0_writeback_mshr)
  assert(!writeback_target_mshr.done_w)
  assert(writeback_target_mshr.cnt_w === s0_writeback_cnt)

  when(writeback_req.fire) {
    s0_writeback_cnt := s0_writeback_cnt + 1
    writeback_target_mshr.cnt_w := s0_writeback_cnt + 1
    when(s0_writeback_cnt.andR) { // Last transfer
      writeback_target_mshr.done_w := True
    }
  }

  val s1_writeback_valid = RegInit(False)
  val s1_writeback_just_sent = RegNext(s0_writeback_send)
  val s1_writeback_data = writeback_resp.readout.clone()
  s1_writeback_data := Mux(s1_writeback_just_sent, writeback_resp.readout, RegNext(s1_writeback_data))
  val s1_writeback_bitmask = RegNextWhen(s0_writeback_bitmask, s0_writeback_send)
  val s1_writeback_muxed = MuxOH(s1_writeback_bitmask, s1_writeback_data.as(Vec(Bits(external_bus.params.data_width bits), transfer_width_ratio)))

  external_bus.downlink.valid := s1_writeback_valid
  external_bus.downlink.payload.data := s1_writeback_muxed
  val s1_writeback_exit = external_bus.downlink.ready

  when(s1_writeback_exit) {
    s1_writeback_valid := writeback_req.fire
  }

  s0_writeback_send := writeMSHRIdxFifo.io.pop.valid && (!s1_writeback_valid || s1_writeback_exit)
  writeMSHRIdxFifo.io.pop.ready := s0_writeback_cnt.andR && (!s1_writeback_valid || s1_writeback_exit)

  /**
    * Refill, 1 stage:
    */
  val refill_id = external_bus.uplink.payload.id
  val refill_target_mshr = mshrs(refill_id)

  val refill_space_allowed = (
    !refill_target_mshr.has_w || refill_target_mshr.cnt_w =/= refill_target_mshr.cnt_r || refill_target_mshr.done_w
  )

  val refill_cnt = UInt(log2Up(cfg.l2.base.line_size * 8 / external_bus.params.data_width) bits)

  refill_req.valid := refill_space_allowed && external_bus.uplink.valid
  // FIXME: finish refill_req

  external_bus.uplink.ready := refill_space_allowed && external_bus.uplink.ready

  assert(!refill_target_mshr.valid)
  assert(refill_target_mshr.cnt_r === refill_cnt)

  when(refill_req.fire) {
    refill_cnt := refill_cnt + 1
    refill_target_mshr.cnt_w := refill_cnt + 1
    when(refill_cnt.andR) { // Last transfer
      assert(!refill_target_mshr.has_w || refill_target_mshr.done_w)
      refill_target_mshr.valid := False
    }
  }

  // TODO: finish return request
}