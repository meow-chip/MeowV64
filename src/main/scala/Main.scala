import core.Core

object Main extends App {
  chisel3.Driver.execute(args, () => new Core)
}