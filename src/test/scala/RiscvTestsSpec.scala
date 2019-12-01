import core._
import util._
import chisel3._
import chisel3.iotesters.PeekPokeTester
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File

object RiscvTestsSpec {
  val cases = new File("./testcases/riscv-tests/build/isa").listFiles
    .filter(_.isFile).filter(_.getName.endsWith(".hex"))
    .map(_.getPath).toList
}

class RiscvTestsSpec extends FlatSpec with Matchers {
  behavior of "RiscvTestsSpec"

  for (file <- RiscvTestsSpec.cases) {
    it should s"run $file successfully" in { ExecTest.runFile(file) should be(true) }
  }
}
