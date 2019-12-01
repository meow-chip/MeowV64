import core._
import util._
import chisel3._
import chisel3.iotesters.PeekPokeTester
import org.scalatest.FlatSpec
import org.scalatest.Matchers

object ExecDef extends CoreDef {
  override val INIT_VEC = BigInt(0)
}

class WrappedCore(coredef: CoreDef, ifile: String) extends Module {
  val io = IO(new Bundle {
    val ended = Output(Bool())
  })

  val core = Module(new Core(coredef))

  val mem = Module(new AXIMem(Some(ifile), 65536, core.coredef.ADDR_WIDTH))

  core.io.axi <> mem.io.axi

  // Exit vector
  io.ended := core.io.pc === 0x100000.U // Stack base
}

class ExecTest(dut: WrappedCore) extends PeekPokeTester(dut) {
  def doTest(bound: Int): Unit = {
    for(i <- (0 until bound)) {
      // println("Cycle: " + i)
      step(1)

      if(peek(dut.io.ended) == 1) {
        println(s"> Process ended at cycle ${i}")
        return
      }
    }

    throw new Error(s"Did not finished within ${bound} cycles")
  }

  doTest(10000)
}

object ExecTest {
  def runFile(file: String, args: Option[Array[String]] = None): Boolean = {
    args match {
      case None => chisel3.iotesters.Driver(
        () => new WrappedCore(ExecDef, file),
        "verilator"
      ) { new ExecTest(_) }

      case Some(args) => chisel3.iotesters.Driver.execute(
        args,
        () => new WrappedCore(ExecDef, file)
      ) { new ExecTest(_) }
    }
  }
}

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
