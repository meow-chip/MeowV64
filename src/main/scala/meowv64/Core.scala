package meowv64

import chisel3._
import chisel3.util._
import meowv64.mem._
import meowv64.config._
import meowv64.frontend._

class Core(implicit cfg: CoreConfig) extends Module {
  val mem_fe = IO(new MemBus(cfg.membus_params(Frontend)))
  val mem_be = IO(new MemBus(cfg.membus_params(Backend)))
  val mem_uc = IO(new MemBus(cfg.membus_params(Uncached)))

  val eint = IO(Input(Bool())) // From PLIC
  val sint = IO(Input(Bool())) // From CLINT

  val frontend = Module(new Frontend)

  frontend.mem <> mem_fe
  frontend.branch.valid := false.B
  frontend.branch.bits := DontCare

  mem_be := DontCare
  mem_be.cmd.valid := false.B
  mem_be.downlink.valid := false.B
  mem_be.ack.valid := false.B
  mem_be.uplink.ready := false.B
  mem_be.inv.ready := false.B

  mem_uc := DontCare
  mem_uc.cmd.valid := false.B
  mem_uc.downlink.valid := false.B
  mem_uc.uplink.ready := false.B
}