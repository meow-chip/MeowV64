import core._
import util._
import chisel3._
import chisel3.iotesters.PeekPokeTester
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.ConfigMap
import java.io.File

object RiscvTestsSpec {
  val knownFails = Seq("rv64-mi-p-scall.bin")
  val cases = new File("./testcases/riscv-tests/isa").listFiles
    .filter(_.isFile).filter(_.getName.endsWith(".bin"))
    .filter(f => knownFails.foldLeft(true)((ret, cur) => ret && !f.getName().endsWith(cur)))
    .map(_.getPath).toList
}

class RiscvTestsSpec extends FlatSpec with Matchers {
  behavior of "RiscvTestsSpec"

  for (file <- RiscvTestsSpec.cases) {
    it should s"run $file successfully" in { ExecTest.runFile(file) should be(true) }
  }
}

object RiscvTestsMain extends App {
  (new RiscvTestsSpec).execute(stats = true, shortstacks = true)
}
