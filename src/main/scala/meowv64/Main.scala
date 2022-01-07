package meowv64

import meowv64.multicore.Multicore
import chisel3.stage.ChiselGeneratorAnnotation
object Main extends App {
  (new chisel3.stage.ChiselStage()).execute(
    Array("-X", "mverilog") ++ args,
    Seq(ChiselGeneratorAnnotation(() => new Multicore))
  )
}
