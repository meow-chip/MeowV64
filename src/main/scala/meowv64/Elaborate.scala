package meowv64

import spinal.core._
import meowv64._
import meowv64.config._

class AXI4Wrapper(cfg: MulticoreConfig) extends Component {
  val eints = in Bits(cfg.eint_cnt bits)
  val mem = new Bundle {
    val awid = out UInt(4 bits)
    val awvalid = out Bool()
    val awready = in Bool()
    val awaddr = out UInt(64 bits)
    val awlen = out UInt(8 bits)
    val awburst = out Bits(2 bits)
    val awsize = out UInt(3 bits)

    val wvalid = out Bool()
    val wready = in Bool()
    val wdata = out Bits(64 bits)
    val wstrb = out Bits(8 bits)
    val wlast = out Bool()

    val bid = out UInt(4 bits)
    val bvalid = in Bool()
    val bready = out Bool()
    val bresp = in Bits(2 bits)

    val arid = out UInt(4 bits)
    val arvalid = out Bool()
    val arready = in Bool()
    val araddr = out UInt(64 bits)
    val arsize = out UInt(3 bits)
    val arlen = out UInt(8 bits)
    val arburst = out Bits(2 bits)

    val rid = out UInt(4 bits)
    val rvalid = in Bool()
    val rready = out Bool()
    val rdata = in Bits(64 bits)
    val rresp = in Bits(2 bits)
    val rlast = in Bool()
  }

  val impl = new Multicore(cfg)
  val axi4 = impl.mem.toAxi4

  mem.awvalid <> axi4.aw.valid
  mem.awready <> axi4.aw.ready
  mem.awid <> axi4.aw.payload.id
  mem.awaddr <> axi4.aw.payload.addr
  mem.awlen <> axi4.aw.payload.len
  mem.awburst <> axi4.aw.payload.burst
  mem.awsize <> axi4.aw.payload.size

  mem.wvalid <> axi4.w.valid
  mem.wready <> axi4.w.ready
  mem.wdata <> axi4.w.payload.data
  mem.wstrb <> axi4.w.payload.strb
  mem.wlast <> axi4.w.payload.last

  mem.bvalid <> axi4.b.valid
  mem.bready <> axi4.b.ready
  mem.bid <> axi4.b.payload.id
  mem.bresp <> axi4.b.resp

  mem.arvalid <> axi4.ar.valid
  mem.arready <> axi4.ar.ready
  mem.arid <> axi4.ar.payload.id
  mem.araddr <> axi4.ar.payload.addr
  mem.arlen <> axi4.ar.payload.len
  mem.arburst <> axi4.ar.payload.burst
  mem.arsize <> axi4.ar.payload.size

  mem.rvalid <> axi4.r.valid
  mem.rready <> axi4.r.ready
  mem.rid <> axi4.r.payload.id
  mem.rdata <> axi4.r.payload.data
  mem.rresp <> axi4.r.payload.resp
  mem.rlast <> axi4.r.payload.last

  impl.eints <> eints
}

object ElaborationConfig extends SpinalConfig()

object Elaborate extends App {
  override def main(args: Array[String]): Unit = {
    ElaborationConfig.generateSystemVerilog(new Multicore(DefaultMulticoreConfig))
    ElaborationConfig.generateSystemVerilog(new AXI4Wrapper(DefaultMulticoreConfig))
  }
}