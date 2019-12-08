import core._
import util._
import chisel3._
import chisel3.iotesters.PeekPokeTester
import org.scalatest.FlatSpec
import org.scalatest.Matchers

object ExecSpec {
  val cases = List(
    ("OP-IMM instructions", "./testcases/meow/bin/op-imm.bin"),
    ("Load/Store", "./testcases/meow/bin/load-store.bin"),
    ("Write-merge", "./testcases/meow/bin/write-merge.bin"),
    ("Unconditional jumps", "./testcases/meow/bin/jump.bin"),
    ("Branches", "./testcases/meow/bin/branch.bin"),
    ("Serial output", "./testcases/meow/bin/serial.bin"),
    ("Multiply", "./testcases/meow/bin/mul.bin"),
    ("Multiply neg", "./testcases/meow/bin/mul-neg.bin"),
    ("Division", "./testcases/meow/bin/div.bin"),
    ("Division neg", "./testcases/meow/bin/div-neg.bin"),
    ("Div by 0 & overflow", "./testcases/meow/bin/div-special.bin"),
    ("Fibonacci", "./testcases/meow/bin/fib.bin")
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
