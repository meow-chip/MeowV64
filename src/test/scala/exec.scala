import core._
import util._
import chisel3._
import chisel3.iotesters.PeekPokeTester
import org.scalatest.FlatSpec
import org.scalatest.Matchers

object ExecDef extends CoreDef {
  override val INIT_VEC = BigInt(0)
}

class ExecTest(dut: Core, ifile: String) extends PeekPokeTester(dut) {
  val imem = Module(new AXIMem(Some(ifile), 65536, dut.coredef.ADDR_WIDTH))

  // val dmem = Module(new AXIMem(None, 65536, dut.coredef.ADDR_WIDTH))

  dut.io.axi <> imem.io.axi
}

class ExecTestMain extends FlatSpec with Matchers {
  behavior of "ExecTest"

  def runFile(file: String): Boolean = {
    chisel3.iotesters.Driver(() => new Core(ExecDef)) { new ExecTest(_, file) }
  }

  it should "run OP-IMM instructions successfully" in { runFile("./testcases/bin/op-imm.bin") should be(true) }
}
