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

  val imem = Module(new AXIMem(Some(ifile), 65536, core.coredef.ADDR_WIDTH))
  val dmem = Module(new AXIMem(None, 65536, core.coredef.ADDR_WIDTH, Some(BigInt(0x100000))))

  core.io.iaxi <> imem.io.axi
  core.io.daxi <> dmem.io.axi

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
      case None => chisel3.iotesters.Driver(() => new WrappedCore(ExecDef, file)) { new ExecTest(_) }
      case Some(a) => chisel3.iotesters.Driver.execute(a, () => new WrappedCore(ExecDef, file)) { new ExecTest(_) }
    }
  }
}

class ExecSpec extends FlatSpec with Matchers {
  behavior of "ExecTest"

  it should "run OP-IMM instructions successfully" in { ExecTest.runFile("./testcases/hex/fib.hex") should be(true) }
}

object ExecTestMain extends App {
  ExecTest.runFile("./testcases/hex/fib.hex", Some(args))
}
