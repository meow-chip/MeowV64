import core._
import util._
import chisel3._
import chisel3.iotesters.PeekPokeTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

object ExecSpec {
  val cases = List(
    ("OP-IMM instructions", "./testcases/meow/bin/op-imm.bin"),
    ("Load/Store", "./testcases/meow/bin/load-store.bin"),
    ("Uncached Load/Store", "./testcases/meow/bin/load-store-uncached.bin"),
    ("Write-merge", "./testcases/meow/bin/write-merge.bin"),
    ("Unconditional jumps", "./testcases/meow/bin/jump.bin"),
    ("Branches", "./testcases/meow/bin/branch.bin"),
    ("Serial output", "./testcases/meow/bin/serial.bin"),
    ("Multiply", "./testcases/meow/bin/mul.bin"),
    ("Multiply neg", "./testcases/meow/bin/mul-neg.bin"),
    ("Division", "./testcases/meow/bin/div.bin"),
    ("Division neg", "./testcases/meow/bin/div-neg.bin"),
    ("Div by 0 & overflow", "./testcases/meow/bin/div-special.bin"),
    ("Page Table - Basic", "./testcases/meow/bin/paging-basic.bin"),
    // ("Fibonacci", "./testcases/meow/bin/fib.bin"),
    ("Timer interrupt", "./testcases/meow/bin/timer.bin"),
    ("External interrupt", "./testcases/meow/bin/eint.bin"),
    ("RAS", "./testcases/meow/bin/ras.bin")
  )
}

class ExecSpec extends AnyFlatSpec with Matchers {
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
