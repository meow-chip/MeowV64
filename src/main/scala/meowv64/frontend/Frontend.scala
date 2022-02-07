package meowv64.frontend

import spinal.core._
import spinal.lib._
import meowv64.config._
import meowv64.mem._

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
    * - FENCE.I
    */
  val halt = Bool

  // TODO: halt reason
}

/**
  * Frontend root class
  */
class Frontend(implicit cfg: CoreConfig) {
  // IOs
  val mem = master (new MemBus(cfg.membus_params(Frontend)))
  val branch = slave Flow (cfg.rint)

  /**
    * Current PC
    */
  val pc = Reg(new PCGen)
  pc.halt init(False)
  pc.pc init(cfg.init_vec)

  /**
    * Pipeline flow
    */
  val flow = Bool()

  /**
    * Pause fetch (insufficient space in fetch queue)
    */
  val bubble = Bool()

  /**
    * Stage valids
    */
  val s0_valid = !bubble
  val s1_valid = RegInit(False)
  val s2_valid = RegInit(False)
  val s3_valid = RegInit(False)

  /**
    * Kill signals
    */
  val kill_s1 = Bool()
  val kill_s2 = Bool()
  val kill_s3 = Bool()

  s1_valid := Mux(flow, s0_valid, s1_valid && !kill_s1)
  s2_valid := Mux(flow, s1_valid && !kill_s1, s2_valid && !kill_s2)
  s3_valid := Mux(flow, s2_valid && !kill_s2, s3_valid && !kill_s3)

  ////////////////////
  // ICache
  ////////////////////

  /**
    * ICache has three stage, so at s2 we can get the result
    */
  val s0_pc_offset = (pc.pc >> log2Up(Consts.INSTR_WIDTH))(0, log2Up(cfg.fetch_width) bits)
  val s0_pc_aligned = (pc.pc >> log2Up(Consts.INSTR_WIDTH * cfg.fetch_width)) ## U(0, log2Up(Consts.INSTR_WIDTH * cfg.fetch_width) bits)
  val s0_pc_mask = B(BigInt(2).pow(cfg.fetch_width) - 1, cfg.fetch_width bits) << s0_pc_offset

  val s1_pc_aligned = RegNextWhen(s0_pc_aligned, flow)

  val cache = new InstrCache
  cache.mem <> mem
  cache.input.s0_vaddr.payload := s0_pc_aligned.asUInt
  cache.input.s0_vaddr.valid := s0_valid && !pc.halt // If halt, don't need to send fetch to ICache
  cache.input.s1_paddr := s1_pc_aligned.asUInt

  ////////////////////
  // BPU
  ////////////////////
  val bpu = new BPU
  bpu.pause := !flow
  bpu.pc.base := s0_pc_aligned.asUInt
  bpu.pc.mask := s0_pc_mask

  ////////////////////
  // NPC Arbitration
  ////////////////////
  
  /**
    * NPC arbitration takes from following sources in decreasing priority:
    * - Mispredicted branch
    * - BPU (4 cycle delay) + Default prediction (after pre-decode) (at s3)
    * - BPU (3 cycle delay) (at s2)
    * - BPU (2 cycle delay) (at s1)
    * - BPU (NLP) (at s0)
    * 
    * Additionally, if there is pending interrupt, halt is set to true no matter what
    */
  val npc = PriorityMux(Seq(
    branch.valid -> branch.payload,
    bpu.preds.d4.valid -> bpu.preds.d4.payload, // TODO: Default prediction
    bpu.preds.d3.valid -> bpu.preds.d3.payload,
    bpu.preds.d2.valid -> bpu.preds.d2.payload,
    True -> bpu.preds.d1,
  ))

  val npc_halt = False // TODO: impl

  kill_s1 := bpu.preds.d3.valid || bpu.preds.d4.valid || branch.valid
  kill_s2 := bpu.preds.d4.valid || branch.valid
  kill_s3 := branch.valid

  when(flow) {
    pc.pc := npc
    pc.halt := npc_halt
  }
}
