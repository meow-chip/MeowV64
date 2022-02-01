package meowv64

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.Axi4
import meowv64.config.MulticoreConfig
import spinal.lib.bus.amba4.axi.Axi4Config

class ExtMemConfig(cfg: MulticoreConfig) extends Axi4Config(
  addressWidth = 48, // TODO: make this configurable
  dataWidth = 64 , // TODO: make this configurable
  idWidth = 4,
  useRegion = false,
  useLock = false,
  useCache = false,
  useQos = false,
  useProt = false,
)

class Multicore(cfg: MulticoreConfig) extends Component {
  val mem = master (new Axi4(new ExtMemConfig(cfg)))
  val eints = in Bits(cfg.eint_cnt bits)
}