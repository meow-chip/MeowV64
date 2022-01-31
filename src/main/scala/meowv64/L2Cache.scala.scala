package meowv64

import spinal.core._
import spinal.lib._
import meowv64._
import meowv64.config._

/**
  * This is a stub. You can extend it by feeding meow
  */

class L2Cache(implicit cfg: MulticoreConfig) extends Component {
  val to_cores = cfg.cores.map(core => new Bundle {
    val frontend = slave (new MemBus(core.frontend_membus_params, true))
  })
}