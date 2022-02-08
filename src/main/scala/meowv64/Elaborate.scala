package meowv64

import chisel3._
import meowv64._
import meowv64.config._
import meowv64.mem._

class AXI4Wrapper(implicit cfg: MulticoreConfig) extends Module {
  val eints = IO(Input(Bits(cfg.eint_cnt.W)))

  val mem = new Axi4(cfg.membus_params(External).to_axi_config)

  val impl = new Multicore

  mem <> impl.mem.toAxi4
  impl.eints <> eints
}

/*
object ElaborationConfig extends ChiselConfig()

object Elaborate extends App {
  override def main(args: Array[String]): Unit = {
    ElaborationConfig.generateSystemVerilog(new Multicore()(DefaultMulticoreConfig))
    ElaborationConfig.generateSystemVerilog(new AXI4Wrapper()(DefaultMulticoreConfig))
  }
}
*/