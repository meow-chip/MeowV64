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
  })

  val core = Module(new Core(coredef))

  val imem = Module(new AXIMem(Some(ifile), 65536, core.coredef.ADDR_WIDTH))
  // val dmem = Module(new AXIMem(None, 65536, dut.coredef.ADDR_WIDTH))

  core.io.axi <> imem.io.axi
}

class ExecTest(dut: WrappedCore) extends PeekPokeTester(dut) {
  for(i <- (0 until 12)) {
    step(1)
  }
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

  it should "run OP-IMM instructions successfully" in { ExecTest.runFile("./testcases/hex/op-imm.hex") should be(true) }
}

object ExecTestMain extends App {
  ExecTest.runFile("./testcases/hex/op-imm.hex", Some(args))
}
