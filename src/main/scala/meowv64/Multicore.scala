package meowv64

import spinal.core._
import spinal.lib._
import meowv64.mem._
import meowv64.config._
import meowv64.l2._

class Multicore(implicit cfg: MulticoreConfig) extends Component {
  val mem = master (new MemBus(cfg.membus_params(External)))
  val eints = in Bits(cfg.eint_cnt bits)

  val cores = cfg.cores.map(
    implicit cfg => new Core
  )
  val l2 = new L2Cache
  val core_ucs = for((c, b) <- cores.zip(l2.internal_bus)) yield {
    c.mem_fe <> b.frontend
    c.mem_be <> b.backend

    c.mem_uc
  }

  // TODO: intc(l2, cores.map(_.uc))
  mem <> l2.external_bus
}