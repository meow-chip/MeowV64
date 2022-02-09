package meowv64

import chisel3._
import meowv64.mem._
import meowv64.config._
import meowv64.l2._

class Multicore(implicit cfg: MulticoreConfig) extends Module {
  val mem = IO(new MemBus(cfg.membus_params(External)))
  val eints = IO(Input(UInt(cfg.eint_cnt.W)))

  val cores = cfg.cores.map(
    implicit cfg => Module(new Core)
  )
  val l2 = Module(new L2Cache)
  val core_ucs = for((c, b) <- cores.zip(l2.internal_bus)) yield {
    c.mem_fe <> b.frontend
    c.mem_be <> b.backend

    c.mem_uc
  }

  // TODO: intc(l2, cores.map(_.uc))
  for(u <- core_ucs) {
    u := DontCare
    u.cmd.ready := false.B
    u.downlink.ready := false.B
    u.uplink.valid := false.B
  }

  mem <> l2.external_bus

  // TODO: CLINT & PLIC
  for(c <- cores) {
    c.eint := false.B
    c.sint := false.B
  }
}