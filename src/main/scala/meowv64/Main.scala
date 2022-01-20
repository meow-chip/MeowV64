package meowv64

import chisel3.stage.ChiselGeneratorAnnotation
import firrtl.options.Dependency
import firrtl.stage.RunFirrtlTransformAnnotation
import meowv64.system.System
import meowv64.system.SystemDef

object Main extends App {
  val (className, rest) = if (args.length > 0) {
    (args.head, args.tail)
  } else {
    ("meowv64.system.DefaultSystemDef", args)
  }

  println(s"Using config ${className}")
  val conf = Class
    .forName(className)
    .getDeclaredConstructor()
    .newInstance()
    .asInstanceOf[SystemDef]
  (new chisel3.stage.ChiselStage()).execute(
    Array("-X", "mverilog") ++ rest,
    Seq(
      ChiselGeneratorAnnotation(() => new System()(conf)),
      RunFirrtlTransformAnnotation(Dependency(ZeroInit))
    )
  )
}
