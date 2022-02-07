package meowv64

import spinal.core._
import spinal.lib._
import meowv64.mem._
import meowv64.config._
import meowv64.frontend._

class Core(implicit cfg: CoreConfig) extends Component {
  val mem_fe = master (new MemBus(cfg.membus_params(Frontend)))
  val mem_be = master (new MemBus(cfg.membus_params(Backend)))
  val mem_uc = master (new MemBus(cfg.membus_params(Uncached)))

  val eint = in Bool()
  val sint = in Bool() // From CLINT

  val frontend = new Frontend

  frontend.mem <> mem_fe
  frontend.branch.valid := False
  frontend.branch.payload.assignDontCare()

  mem_be.assignDontCare()
  mem_be.cmd.valid := False
  mem_be.downlink.valid := False
  mem_be.ack.valid := False
  mem_be.uplink.ready := False
  mem_be.inv.ready := False

  mem_uc.assignDontCare()
  mem_uc.cmd.valid := False
  mem_uc.downlink.valid := False
  mem_uc.ack.valid := False
  mem_uc.uplink.ready := False
  mem_uc.inv.ready := False
}