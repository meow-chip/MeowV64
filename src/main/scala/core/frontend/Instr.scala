package core.frontend

import core.Config
import chisel3._

/**
 * An instruction after pre-decode
 */
class Instr extends Bundle {
  val raw = UInt(32.W) // After expansion
  val isBr = Bool()
  val isCall = Bool()
  val isRet = Bool()
}

object Instr {
  def predecode(raw: UInt): Instr = {
    // FIXME: impl
    ???
  }
}

/**
 * A fetch packet without compression
 */
class FetchPacket(implicit val config: Config) extends Bundle {
  val content = Vec(config.fetchWidth, new Bundle {
    val valid = Bool()
    val instr = new Instr
  })
}