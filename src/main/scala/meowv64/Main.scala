package meowv64

import meowv64.multicore.Multicore
import chisel3.stage.ChiselGeneratorAnnotation
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.options.Dependency

object Main extends App {
  (new chisel3.stage.ChiselStage()).execute(
    Array("-X", "mverilog") ++ args,
    Seq(
      ChiselGeneratorAnnotation(() => new Multicore),
      RunFirrtlTransformAnnotation(Dependency(ZeroInit))
    )
  )
}
