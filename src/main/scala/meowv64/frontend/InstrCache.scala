package meowv64.frontend

import spinal.core._
import meowv64.config.CoreConfig
import meowv64.config.Consts

// TODO: we are presumably giving ICache a 3 cycle delay:
// Input -> Stage 0 | Stage 1 | Stage 2 | Stage 3 -> Output
class InstrCache(implicit cfg: CoreConfig) extends Component {
  val pc = in (cfg.rint)
  val output = out (FetchVec)

  assert(cfg.ic.offset(pc) === U(0), "PC should be aligned")

  // Kills doesn't reflect on output validity
  // E.g., s3 := true doens't causes output valid to drop
  val kills = new Bundle {
    val s1 = in Bool _
    val s2 = in Bool _
    val s3 = in Bool _
  }

  // Some helper
  def FetchVec = Vec(Consts.ibits, cfg.fetch_width)
  def LineVec = Vec(
    FetchVec,
    cfg.ic.line_width * 8 / cfg.fetch_width
  )

  // Valid matrix
  val valids = RegInit(Vec(
    Vec(False, cfg.ic.assoc_cnt),
    cfg.ic.line_per_assoc,
  ))

  // Tag memory
  val tags = Mem(
    Vec(Bits(cfg.ic.tag_width(cfg.xlen) bits), cfg.ic.assoc_cnt),
    cfg.ic.line_per_assoc,
  )

  // Data memory
  val data = Mem(
    Vec(LineVec, cfg.ic.assoc_cnt),
    cfg.ic.line_per_assoc,
  )

  ////////////////
  // Stage 0 + Stage 1
  ////////////////

  /**
    * Stage 0 sends reads to all memory
    */
  val s0_idx = cfg.ic.index(pc)
  val s1_tags = tags.readSync(s0_idx)
  val s1_data = data.readSync(s0_idx)
}