package meowv64

import chisel3.stage.ChiselGeneratorAnnotation
import firrtl.options.Dependency
import firrtl.stage.RunFirrtlTransformAnnotation
import meowv64.multicore.Multicore

object Main extends App {
  (new chisel3.stage.ChiselStage()).execute(
    Array("-X", "mverilog") ++ args,
    Seq(
      ChiselGeneratorAnnotation(() => new Multicore),
      RunFirrtlTransformAnnotation(Dependency(ZeroInit))
    )
  )
}
