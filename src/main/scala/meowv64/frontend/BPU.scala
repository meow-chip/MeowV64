package meowv64.frontend

import spinal.core._
import meowv64.config.CoreConfig
import spinal.lib._
import meowv64.config.Consts

class BPU(implicit cfg: CoreConfig) extends Component {
  val pc = new Bundle {
    // Current fetch PC, instruction in ICache s0
    val base = in (cfg.rint)
    val mask = in Bits (cfg.fetch_width bits)
    // TODO: mask
  }

  val pause = in Bool() // Pipeline pause

  val preds = new Bundle {

    // NPC, fetch PC when instruction in ICache s1
    val d1 = out (cfg.rint)

    // PC delayed by 2 cycle, instruction in ICache s2, causes ICache to kill s1
    val d2 = master Flow (cfg.rint)

    // NPC delayed in 3 cycle, instruction in ICache s3, causes ICache to kill s1 and s2
    val d3 = master Flow (cfg.rint)

    // NPC delayed in 4 cycle, instruction after predecode, causes ICache to kill s1, s2 and s3
    // TODO: do we need this?
    val d4 = master Flow (cfg.rint)
  }

  preds.d1 := pc.base + Consts.INSTR_WIDTH / 8 * cfg.fetch_width

  preds.d2.valid := False
  preds.d3.valid := False
  preds.d4.valid := False
}