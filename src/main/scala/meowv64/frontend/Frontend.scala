package meowv64.frontend

import chisel3._
import chisel3.util._
import meowv64.config._
import meowv64.mem._
import chisel3.util.DecoupledIO
import chisel3.util.Valid

class PCGen(implicit cfg: CoreConfig) extends Bundle {
  /**
    * PC sent to I$ in the current cycle
    */
  val pc = UInt(cfg.xlen.W)

  /**
    * If we need to halt I$ fetch, e.g.:
    * - An fetch exception occurred
    * - Interrupts
    * - WFI
    * - FENCE.I
    */
  val halt = Bool()

  // TODO: halt reason
}

/**
  * Frontend root class
  */
class Frontend(implicit cfg: CoreConfig) extends Module {
  // IOs
  val mem = IO(new MemBus(cfg.membus_params(Frontend)))
  val branch = IO(Flipped(Valid(cfg.rint)))

  /**
    * Current PC
    */
  val pc = RegInit({
    val init = Wire(new PCGen)
    init.halt := false.B
    init.pc := cfg.init_vec.U
    init
  })

  /**
    * Pipeline flow
    */
  val flow = Wire(Bool())

  /**
    * Pause fetch (insufficient space in fetch queue)
    */
  val bubble = Wire(Bool())

  /**
    * Stage valids
    */
  val s0_valid = !bubble
  val s1_valid = RegInit(false.B)
  val s2_valid = RegInit(false.B)
  val s3_valid = RegInit(false.B)

  /**
    * Kill signals
    */
  val kill_s1 = Wire(Bool())
  val kill_s2 = Wire(Bool())
  val kill_s3 = Wire(Bool())

  s1_valid := Mux(flow, s0_valid, s1_valid && !kill_s1)
  s2_valid := Mux(flow, s1_valid && !kill_s1, s2_valid && !kill_s2)
  s3_valid := Mux(flow, s2_valid && !kill_s2, s3_valid && !kill_s3)

  ////////////////////
  // ICache
  ////////////////////

  /**
    * ICache has three stage, so at s2 we can get the result
    */
  val s0_pc_offset = (pc.pc >> log2Ceil(Consts.INSTR_WIDTH / 8))(log2Ceil(cfg.fetch_width) - 1, 0)
  val s0_pc_aligned = (pc.pc >> log2Ceil(Consts.INSTR_WIDTH / 8 * cfg.fetch_width)) ## 0.U(log2Ceil(Consts.INSTR_WIDTH / 8 * cfg.fetch_width).W)
  val s0_pc_mask = (BigInt(2).pow(cfg.fetch_width) - 1).U(cfg.fetch_width.W) << s0_pc_offset

  val s1_pc_aligned = RegEnable(s0_pc_aligned, flow)

  val cache = Module(new InstrCache)
  cache.mem <> mem
  cache.input.s0_vaddr.bits := s0_pc_aligned.asUInt
  cache.input.s0_vaddr.valid := s0_valid && !pc.halt // If halt, don't need to send fetch to ICache
  cache.input.s1_paddr := s1_pc_aligned.asUInt

  flow := !cache.output.paused

  // TODO: impl bubble
  bubble := false.B
  cache.kills.s1 := kill_s1
  cache.kills.s2 := kill_s2

  ////////////////////
  // BPU
  ////////////////////
  val bpu = Module(new BPU)
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
    branch.valid -> branch.bits,
    bpu.preds.d4.valid -> bpu.preds.d4.bits, // TODO: Default prediction
    bpu.preds.d3.valid -> bpu.preds.d3.bits,
    bpu.preds.d2.valid -> bpu.preds.d2.bits,
    true.B -> bpu.preds.d1,
  ))

  val npc_halt = false.B // TODO: impl

  kill_s1 := bpu.preds.d3.valid || bpu.preds.d4.valid || branch.valid
  kill_s2 := bpu.preds.d4.valid || branch.valid
  kill_s3 := branch.valid

  when(flow) {
    pc.pc := npc
    pc.halt := npc_halt
  }
}
