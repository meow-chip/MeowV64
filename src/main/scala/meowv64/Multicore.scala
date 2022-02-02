package meowv64

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.Axi4
import meowv64.config.MulticoreConfig
import spinal.lib.bus.amba4.axi.Axi4Config
import meowv64.mem._

class Multicore(cfg: MulticoreConfig) extends Component {
  val mem = master (new MemBus(cfg.membus_params(External)))
  val eints = in Bits(cfg.eint_cnt bits)
  // FIXME: impl
}