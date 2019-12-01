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
    ("Unconditional jumps", "./testcases/moew/hex/jump.hex"),
    ("Branches", "./testcases/moew/hex/branch.hex"),
    ("Serial output", "./testcases/moew/hex/serial.hex"),
    ("Multiply", "./testcases/moew/hex/mul.hex"),
    ("Multiply neg", "./testcases/moew/hex/mul-neg.hex"),
    ("Division", "./testcases/moew/hex/div.hex"),
    ("Division neg", "./testcases/moew/hex/div-neg.hex"),
    ("Div by 0 & overflow", "./testcases/moew/hex/div-special.hex"),
    ("Fibonacci", "./testcases/moew/hex/fib.hex")
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
