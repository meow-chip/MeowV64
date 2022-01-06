package meowv64

import meowv64.multicore.Multicore
object Main extends App {
  chisel3.Driver.execute(args, () => new Multicore)
}
