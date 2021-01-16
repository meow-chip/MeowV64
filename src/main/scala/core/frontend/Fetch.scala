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
  val pcAligned = pc(config.xlen-1, config.fetchOffset) ## 0.U(config.fetchOffset.W)
  val fetchOffset = if(config.fetchOffset == 1) {
    0.U
  } else {
    pc(config.fetchOffset-1, 1)
  }

  val icache = Module(new ICache)
  icache.addr.bits := pcAligned
  icache.addr.valid := true.B
  icache.flush := false.B

  val nlp = Module(new NLP)
  nlp.pc := pc
  when(icache.addr.fire()) {
    pcreg := nlp.npc
  }

  when(br.valid) {
    pc := br.bits
    icache.flush := true.B
    // TODO: flush other parts
  }
}