import core._
import util._
import chisel3._
import chisel3.iotesters.PeekPokeTester
import org.scalatest.FlatSpec
import org.scalatest.Matchers

object ExecSpec {
  val cases = List(
    ("OP-IMM instructions", "./testcases/meow/hex/op-imm.hex"),
    ("Load/Store", "./testcases/meow/hex/load-store.hex"),
    ("Write-merge", "./testcases/meow/hex/write-merge.hex"),
    ("Unconditional jumps", "./testcases/meow/hex/jump.hex"),
    ("Branches", "./testcases/meow/hex/branch.hex"),
    ("Serial output", "./testcases/meow/hex/serial.hex"),
    ("Multiply", "./testcases/meow/hex/mul.hex"),
    ("Multiply neg", "./testcases/meow/hex/mul-neg.hex"),
    ("Division", "./testcases/meow/hex/div.hex"),
    ("Division neg", "./testcases/meow/hex/div-neg.hex"),
    ("Div by 0 & overflow", "./testcases/meow/hex/div-special.hex"),
    ("Fibonacci", "./testcases/meow/hex/fib.hex")
  )
}

class ExecSpec extends FlatSpec with Matchers {
  behavior of "ExecSpec"

  for ((desc, file) <- ExecSpec.cases) {
    it should s"run $desc successfully" in { ExecTest.runFile(file) should be(true) }
  }
}

object ExecTestMain extends App {
  val fn = args.last
  println(s"Running $fn...")
  ExecTest.runFile(fn, Some(args))
}
