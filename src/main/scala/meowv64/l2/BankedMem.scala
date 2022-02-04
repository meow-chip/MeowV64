package meowv64.l2

import spinal.core._
import spinal.lib._

case class BankedMemConfig(
  // Total size in bytes
  total_size: Int,

  // Width per access in bytes
  access_size: Int,

  // How many concurrent access can be made
  concurrency: Int,

  // In total, how many subbanks are there in each concurrency bank
  subbank_cnt: Int,

  // How many ports are there. Persumably there is five.
  port_cnt: Int,
) {
  // Byte sizes for all banks combined
  def row_size = access_size * concurrency
  // How many rows are there (depth of each bank)
  def row_cnt = total_size / row_size
  // Bit width of each subbank
  def subbank_size = access_size / subbank_cnt
  // Width of bankidx
  def bankidx_width = log2Up(concurrency)

  def bankidx(idx: UInt) = idx(0, bankidx_width bits)
  def memidx(idx: UInt) = idx >> bankidx_width

  require(isPow2(subbank_cnt))
  require(isPow2(subbank_size))
}

class BankedMemReq(implicit cfg: BankedMemConfig) extends Bundle {
  val idx = UInt(log2Up(cfg.total_size / cfg.access_size) bits)
  val wdata = Bits(cfg.access_size * 8 bits)
  val we = Bool() // Write enable
  val sbe = Bits(cfg.subbank_cnt bits) // Subbank enable
}

class BankedMem(name: String)(implicit cfg: BankedMemConfig) extends Component {
  this.setName(name)

  ////////////////////
  // IOs
  ////////////////////

  val ports = (0 to cfg.port_cnt).map(idx => {
    val port = new Bundle {
      val req = slave Stream(new BankedMemReq)
      val readout = out Bits(cfg.access_size * 8 bits)
    }
    port.setName(s"$name / Port $idx")
    port
  })

  val banks = (0 to cfg.concurrency).map(idx => {
    (0 to cfg.subbank_cnt).map(sidx => {
      val bank = Mem(Bits(cfg.subbank_size * 8 bits), cfg.row_cnt)
      bank.setName(s"$name / Bank $idx.$sidx")
      bank
    })
  })

  // If higer prioritized port is already blocked
  var allowed = True
  // Usage by higher prioritized port
  var used = B(0, cfg.concurrency bits)

  val usage = for(port <- ports) yield {
    val bidx = cfg.bankidx(port.req.payload.idx)
    val bidx_oh = UIntToOh(bidx, cfg.concurrency)
    val can_schedule = !(bidx_oh & used).orR

    used := used | (Vec.fill(0)(port.req.valid).asBits & bidx_oh)
    allowed := allowed & ((!port.req.valid) || can_schedule)

    port.req.ready := can_schedule

    (Vec.fill(0)(port.req.valid && can_schedule).asBits & bidx_oh)
  }

  val s1_readout = for((bank, idx) <- banks.zipWithIndex) yield {
    val port_select = usage.map(_(idx))
    assert(CountOne(port_select) <= 1)
    val port_enable = port_select.orR

    val req = MuxOH(port_select, ports.map(_.req.payload))

    // TODO: lift this
    require(cfg.subbank_cnt == 1)
    bank(0).readWriteSync(cfg.memidx(req.idx), req.wdata, port_enable, req.we)
  }

  val s1_usage = usage.map(RegNext(_))
  for((p, s1_u) <- ports.zip(s1_usage)) {
    p.readout := MuxOH(s1_u.asBools.seq, s1_readout)
  }
}