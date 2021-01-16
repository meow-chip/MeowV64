package core.frontend

import chisel3._
import chisel3.util._
import _root_.core.Config
import _root_.core.cache.ICache

// TODO: figuring out the delay
class Fetch(implicit val config: Config) extends MultiIOModule {
  val br = IO(Flipped(Valid(UInt(config.xlen.W))))
  val fetched = IO(Decoupled(new FetchPacket))

  val pcreg = RegInit(config.initVec.U(config.xlen.W))
  val pc = WireDefault(pcreg)
  val pcAligned = pc(config.xlen-1, log2Ceil(config.fetchWidth) + 1)
  val fetchOffset = if(config.fetchWidth == 1) {
    0.U
  } else {
    pc(log2Ceil(config.fetchWidth), 1)
  }

  val icache = Module(new ICache)
  icache.addr.bits := pcAligned
  icache.addr.valid := true.B
  icache.flush := false.B

  val npc = pcAligned + (config.fetchWidth * 2).U
  when(icache.addr.fire()) {
    pcreg := npc
  }

  when(br.valid) {
    pc := br.bits
    icache.flush := true.B
    // TODO: flush other parts
  }
}