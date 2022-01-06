package meowv64

import meowv64.multicore.Multicore
object Main extends App {
  (new chisel3.stage.ChiselStage()).emitVerilog(new Multicore, args)
}
