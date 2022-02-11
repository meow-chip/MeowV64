package meowv64

import chisel3._
import chisel3.stage._
import meowv64._
import meowv64.config._
import meowv64.mem._

class AXI4Wrapper(implicit cfg: MulticoreConfig) extends Module {
  val eints = IO(Input(Bits(cfg.eint_cnt.W)))

  val mem = new Axi4(cfg.membus_params(External).to_axi_config)

  val impl = Module(new Multicore)

  mem <> impl.mem.toAxi4
  impl.eints <> eints
}

object Elaborate extends App {
  override def main(args: Array[String]): Unit = {
    (new ChiselStage).emitSystemVerilog(new Multicore()(DefaultMulticoreConfig), args)
    // TODO: enable this after finishing toAxi4 conversion
    // (new ChiselStage).emitVerilog(new AXI4Wrapper()(DefaultMulticoreConfig), args)
  }
}