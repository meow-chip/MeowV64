package meowv64.frontend

import spinal.core._
import meowv64.config.CoreConfig

class PCGen(implicit cfg: CoreConfig) extends Bundle {
  /**
    * PC sent to I$ in the current cycle
    */
  val pc = UInt(cfg.xlen bits)

  /**
    * If we need to halt I$ fetch, e.g.:
    * - An fetch exception occurred
    * - Interrupts
    * - WFI
    */
  val halt = Bool
}

object PCGen {
  def default(implicit cfg: CoreConfig) = {
    val v = new PCGen
    v.halt := False
    v.pc := U(cfg.init_vec)
    v
  }
}

/**
  * Frontend root class
  */
class Frontend(implicit cfg: CoreConfig) {
  /**
    * Current PC
    */
  val pc = RegInit(PCGen.default)

  /////////////////////////////////////
  // NPC Arbitration
  /////////////////////////////////////
  
  /**
    * NPC arbitration takes from following sources in decreasing priority:
    * - Mispredicted branch
    * - BPU (3 cycle delay) + Default prediction (after pre-decode)
    * - BPU (2 cycle delay)
    * - BPU (NLP)
    * 
    * Additionally, if there is pending interrupt, halt is set to true no matter what
    */
}
