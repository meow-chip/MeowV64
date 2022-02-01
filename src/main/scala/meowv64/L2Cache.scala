package meowv64

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
  require(cfg.l2.base.bank_cnt == 1) // TODO: impl banking

  val internal_bus = cfg.cores.map(core => new Bundle {
    val frontend = slave(new MemBus(core.membus_params(Frontend)))
    val lsu = slave(new MemBus(core.membus_params(Backend)))
  })
  val external_bus = master(new MemBus(cfg.membus_params))

  val banks = for(i <- 0 to cfg.l2.base.bank_cnt) yield new L2Bank(i)
  banks(0).external_bus <> external_bus
  for((l, r) <- banks(0).internal_bus.zip(internal_bus)) l <> r
}

class PendingInv extends Bundle {
  val valid = Bool()

  val is_invalidation = Bool() // If not, is 
  val sent = Bool() // If true, already sent to master, but has pending data write

  val addr = UInt(Consts.MAX_PADDR_WIDTH bits)
}

class PendingWrite(implicit cfg: MulticoreConfig) extends Bundle {
  val valid = Bool()

  val addr = UInt(Consts.MAX_PADDR_WIDTH bits)
  val cnt = UInt()

  val associated_inv = UInt(log2Up(cfg.l2.max_pending_inv) bits)
}

class PendingReadHead(implicit cfg: MulticoreConfig) extends Bundle {
  // val subidx = UInt(log2Up(cfg.l2.base.line_width / cfg.cores(0).membus_params(Frontend).data_width) bits)
  val cnt = UInt(log2Up(cfg.l2.base.line_width / cfg.cores(0).membus_params(Frontend).data_width) bits)
}

class PendingRead(implicit cfg: MulticoreConfig) extends Bundle with MatchedData[UInt] {
  val addr = UInt(Consts.MAX_PADDR_WIDTH bits)
  val idx = UInt(cfg.l2.base.index_width bits)
  val subidx = UInt(log2Up(cfg.l2.base.line_width / cfg.cores(0).membus_params(Frontend).data_width) bits)

  def matched(matcher: UInt): Bool = addr === matcher
}

class L2Bank(bank_id: Int)(implicit cfg: MulticoreConfig) extends Component {
  val internal_bus = cfg.cores.map(core => new Bundle {
    val frontend = slave(new MemBus(core.membus_params(Frontend)))
    val lsu = slave(new MemBus(core.membus_params(Backend)))
  })
  val external_bus = master(new MemBus(cfg.membus_params))

  val src = internal_bus.flatMap(core => Seq(core.frontend, core.lsu))

  // Memories
  val valids = Mem(Bool(), cfg.l2.base.line_per_assoc)
  val dirtys = Mem(Bool(), cfg.l2.base.line_per_assoc)
  val reservations = Mem(Bits(cfg.cores.length bits), cfg.l2.base.line_per_assoc)
  val occupations = Mem(Bits(cfg.cores.length bits), cfg.l2.base.line_per_assoc)
  val master_dirtys = Mem(Bool(), cfg.l2.base.line_per_assoc) // Master is dirty
  val tags = Mem(UInt(cfg.l2.base.tag_width(Consts.MAX_PADDR_WIDTH) bits), cfg.l2.base.line_per_assoc)

  // Pendings
  val pending_writes = src.flatMap(bus => if(bus.params.bus_type.with_write) Some(Reg(new PendingWrite)) else None)
  val pending_invs = src.flatMap(bus => if(bus.params.bus_type.with_coherence) Some(Reg(Vec(new PendingInv, cfg.l2.max_pending_inv))) else None)
  val pending_read_heads = src.map(_ => Reg(new PendingReadHead))
  val pending_reads = src.map(_ => new MatchingQueue[UInt, PendingRead](new PendingRead, UInt(Consts.MAX_PADDR_WIDTH bits), cfg.l2.max_pending_read))
  require(pending_writes.length == cfg.cores.length)

  valids.setName(s"L2 Bank_$bank_id / valids")
  reservations.setName(s"L2 Bank_$bank_id / reservations")
  occupations.setName(s"L2 Bank_$bank_id / occupations")
  tags.setName(s"L2 Bank_$bank_id / tags")

  val cmd_arb = StreamArbiterFactory.roundRobin.noLock.on(src.map(_.cmd)) // 2 * core_cnt sources, idx >> 1 = core src
  val downlink_arb = StreamArbiterFactory.roundRobin.noLock.on(src.map(_.downlink).filter(_ != null)) // core_cnt sources
}