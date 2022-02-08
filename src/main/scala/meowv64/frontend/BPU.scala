package meowv64.frontend

import chisel3._
import meowv64.config.CoreConfig
import chisel3.util._
import meowv64.config.Consts

class BPU(implicit cfg: CoreConfig) extends Module {
  val pc = IO(new Bundle {
    // Current fetch PC, instruction in ICache s0
    val base = Input(cfg.rint)
    val mask = Input(UInt(cfg.fetch_width.W))
    // TODO: mask
  })

  val pause = IO(Input(Bool())) // Pipeline pause

  val preds = IO(new Bundle {

    // NPC, fetch PC when instruction in ICache s1
    val d1 = Output(cfg.rint)

    // PC delayed by 2 cycle, instruction in ICache s2, causes ICache to kill s1
    val d2 = Valid(cfg.rint)

    // NPC delayed in 3 cycle, instruction in ICache s3, causes ICache to kill s1 and s2
    val d3 = Valid(cfg.rint)

    // NPC delayed in 4 cycle, instruction after predecode, causes ICache to kill s1, s2 and s3
    // TODO: do we need this?
    val d4 = Valid(cfg.rint)
  })

  preds.d1 := pc.base + (Consts.INSTR_WIDTH / 8 * cfg.fetch_width).U

  preds.d2.valid := false.B
  preds.d3.valid := false.B
  preds.d4.valid := false.B
}