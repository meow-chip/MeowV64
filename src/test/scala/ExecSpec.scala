import core._
import util._
import chisel3._
import chisel3.iotesters.PeekPokeTester
import org.scalatest.FlatSpec
import org.scalatest.Matchers

object ExecSpec {
  val cases = List(
    ("OP-IMM instructions", "./testcases/hex/op-imm.hex"),
    ("Load/Store", "./testcases/hex/load-store.hex"),
    ("Unconditional jumps", "./testcases/hex/jump.hex"),
    ("Branches", "./testcases/hex/branch.hex"),
    ("Serial output", "./testcases/hex/serial.hex"),
    ("Multiply", "./testcases/hex/mul.hex"),
    ("Multiply neg", "./testcases/hex/mul-neg.hex"),
    ("Division", "./testcases/hex/div.hex"),
    ("Division neg", "./testcases/hex/div-neg.hex"),
    ("Div by 0 & overflow", "./testcases/hex/div-special.hex"),
    ("Fibonacci", "./testcases/hex/fib.hex")
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
