package meowv64

import spinal.core._
import meowv64._
import meowv64.config._

class Wrapper(cfg: MulticoreConfig) extends Component {
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

  mem.awvalid <> impl.mem.aw.valid
  mem.awready <> impl.mem.aw.ready
  mem.awid <> impl.mem.aw.payload.id
  mem.awaddr <> impl.mem.aw.payload.addr
  mem.awlen <> impl.mem.aw.payload.len
  mem.awburst <> impl.mem.aw.payload.burst
  mem.awsize <> impl.mem.aw.payload.size

  mem.wvalid <> impl.mem.w.valid
  mem.wready <> impl.mem.w.ready
  mem.wdata <> impl.mem.w.payload.data
  mem.wstrb <> impl.mem.w.payload.strb
  mem.wlast <> impl.mem.w.payload.last

  mem.bvalid <> impl.mem.b.valid
  mem.bready <> impl.mem.b.ready
  mem.bid <> impl.mem.b.payload.id
  mem.bresp <> impl.mem.b.resp

  mem.arvalid <> impl.mem.ar.valid
  mem.arready <> impl.mem.ar.ready
  mem.arid <> impl.mem.ar.payload.id
  mem.araddr <> impl.mem.ar.payload.addr
  mem.arlen <> impl.mem.ar.payload.len
  mem.arburst <> impl.mem.ar.payload.burst
  mem.arsize <> impl.mem.ar.payload.size

  mem.rvalid <> impl.mem.r.valid
  mem.rready <> impl.mem.r.ready
  mem.rid <> impl.mem.r.payload.id
  mem.rdata <> impl.mem.r.payload.data
  mem.rresp <> impl.mem.r.payload.resp
  mem.rlast <> impl.mem.r.payload.last

  impl.eints <> eints
}

object ElaborationConfig extends SpinalConfig()

object Elaborate extends App {
  override def main(args: Array[String]): Unit = {
    ElaborationConfig.generateSystemVerilog(new Wrapper(DefaultMulticoreConfig))
  }
}