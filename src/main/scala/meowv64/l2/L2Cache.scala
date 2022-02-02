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
    val lsu = slave(new MemBus(core.membus_params(Backend)))
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

class L2Inst(inst_id: Int)(implicit cfg: MulticoreConfig) extends Component {
  val internal_bus = cfg.cores.map(core => new Bundle {
    val frontend = slave(new MemBus(core.membus_params(Frontend)))
    val lsu = slave(new MemBus(core.membus_params(Backend)))
  })
  // TODO: assert they are homogeneous
  val external_bus = master(new MemBus(cfg.membus_params(L2)))

  val src = internal_bus.flatMap(core => Seq(core.frontend, core.lsu))

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

  val writeback_resp = data.ports(4)
  val refill_resp = data.ports(4)
  val invresp_resp = data.ports(4)
  val write_resp = data.ports(4)
  val read_resp = data.ports(4)

  ////////////////////
  // cmd channel
  ////////////////////

  ////////////////////
  // Reading (uplink channel)
  ////////////////////
  val read_output_bp = Bool()
  read_output_bp := False
  val read_streams = for((s, (h, p)) <- src.zip(pending_read_heads.zip(pending_reads))) yield {
    val reflected_stream = Stream(new BankedMemReq)

    reflected_stream.payload.write := False
    reflected_stream.payload.we.assignDontCare()
    reflected_stream.payload.wdata.assignDontCare()
    require(h.cnt.getWidth == p.pop.payload.subidx.getWidth)
    reflected_stream.idx := (h.cnt + p.pop.payload.subidx) + p.pop.payload.idx

    reflected_stream.valid := p.pop.valid
    when(reflected_stream.fire) {
      h.cnt := h.cnt + 1
    }
    p.pop.ready := reflected_stream.fire && h.cnt.andR

    reflected_stream
  }
  val read_arb = new StreamArbiter(new BankedMemReq, read_streams.length)(StreamArbiter.Arbitration.roundRobin, StreamArbiter.Lock.transactionLock)
  (read_arb.io.inputs, read_streams).zipped.foreach(_ << _)
  val read_arb_out = read_arb.io.output
  val read_arb_oh = read_arb.io.chosenOH
  val s1_read_arb_fire = RegNextWhen(read_arb_out.fire, !read_output_bp)
  val s1_read_arb_oh = RegNextWhen(read_arb_oh, !read_output_bp)
  read_output_bp := MuxOH(s1_read_arb_oh, src.map(_.uplink.ready))
  for((uplink, pidx) <- src.map(_.uplink).zipWithIndex) {
    uplink.payload.data := read_resp.asBits
    uplink.valid := s1_read_arb_oh(pidx)
  }

  ////////////////////
  // downlink channel
  ////////////////////

  ////////////////////
  // resp channel
  ////////////////////

  ////////////////////
  // ack channel
  ////////////////////
}