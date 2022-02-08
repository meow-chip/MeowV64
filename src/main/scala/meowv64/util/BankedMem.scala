package meowv64.util

import chisel3._
import chisel3.util._

case class BankedMemConfig(
  // Total size in bytes
  total_size: Int,

  // Width per access in bytes
  access_size: Int,

  // Maximum number of concurrent access when sbe is full
  max_concurrency: Int,

  // In total, how many subbanks are there in each concurrency bank
  subbank_cnt: Int,

  // How many ports are there. Persumably there is five.
  port_cnt: Int,
) {
  // Byte sizes for all banks combined
  def row_size = access_size * max_concurrency
  // How many rows are there (depth of each bank)
  def row_cnt = total_size / row_size
  // Bit width of each subbank
  def subbank_size = access_size / subbank_cnt
  // Width of bankidx
  def bankidx_width = log2Ceil(max_concurrency)

  def bankidx(idx: UInt) = idx(bankidx_width - 1, 0)
  def memidx(idx: UInt) = idx >> bankidx_width

  require(isPow2(subbank_cnt))
  require(isPow2(subbank_size))
}

class BankedMemReq(implicit cfg: BankedMemConfig) extends Bundle {
  val idx = UInt(log2Ceil(cfg.total_size / cfg.access_size).W)
  val wdata = Bits((cfg.access_size * 8).W)
  val we = Bool() // Write enable
  val sbe = Bits(cfg.subbank_cnt.W) // Subbank enable
}

class BankedMem(implicit cfg: BankedMemConfig) extends Module {
  ////////////////////
  // IOs
  ////////////////////

  val ports = (0 until cfg.port_cnt).map(idx => {
    val port = IO(new Bundle {
      val req = Flipped(DecoupledIO(new BankedMemReq))
      val readout = Output(UInt((cfg.access_size * 8).W))
    })
    port.suggestName(s"port.$idx")
    port
  })

  val banks = (0 until cfg.max_concurrency).map(idx => {
    (0 until cfg.subbank_cnt).map(sidx => {
      val bank = SyncReadMem(cfg.row_cnt, UInt((cfg.subbank_size * 8).W))
      bank.suggestName(s"bank.$idx.$sidx")
      bank
    })
  })

  // If higer prioritized port is already blocked
  // TODO: allow non-conflicting subbank reads to be scheduled together
  var allowed = true.B
  // Usage by higher prioritized port
  var used = 0.U((cfg.max_concurrency * cfg.subbank_cnt).W)

  val bidxs = for(port <- ports) yield {
    cfg.bankidx(port.req.bits.idx)
  }

  val usage = for(port <- ports) yield {
    val bidx = cfg.bankidx(port.req.bits.idx)
    val bidx_oh = UIntToOH(bidx, cfg.max_concurrency)

    val req_usage = (
      FillInterleaved(cfg.subbank_cnt, bidx_oh)
      & Fill(cfg.max_concurrency, port.req.bits.sbe)
      & Fill(cfg.max_concurrency * cfg.subbank_cnt, port.req.valid)
    )

    val can_schedule = !(req_usage & used).orR && allowed

    used = used | req_usage
    allowed = allowed & ((!port.req.valid) || can_schedule)

    port.req.ready := can_schedule

    Mux(can_schedule, req_usage, 0.U).asTypeOf(Vec(cfg.max_concurrency, Vec(cfg.subbank_cnt, Bool())))
  }

  val s1_readout = for((bank, bidx) <- banks.zipWithIndex) yield {
    val sreadouts = for((subbank, sbidx) <- bank.zipWithIndex) yield {
      val port_sel_bank = usage.map(_(bidx))
      val port_sel = port_sel_bank.map(_(sbidx))
      val port_en = VecInit(port_sel).asUInt.orR

      assert(PopCount(port_sel) <= 1.U)

      val req = Mux1H(port_sel, ports.map(_.req.bits))
      val port = subbank(cfg.memidx(req.idx))

      val readout = Wire(UInt())
      readout := DontCare
      when(port_en) {
        when(req.we) {
          when(req.sbe(sbidx)) {
            port := req.wdata.asTypeOf(Vec(cfg.subbank_cnt, UInt((cfg.subbank_size * 8).W)))(sbidx)
          }
        }.otherwise {
          readout := port
        }
      }

      readout
    }
    VecInit(sreadouts).asUInt
  }

  val s1_bidxs = bidxs.map(RegNext(_))
  for((p, bidx) <- ports.zip(s1_bidxs)) {
    p.readout := VecInit(s1_readout)(bidx)
  }
}