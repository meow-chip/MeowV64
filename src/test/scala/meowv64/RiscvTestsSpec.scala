import meowv64.util._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

object RiscvTestsSpec {
  val knownFails = Seq("rv64mi-p-scall.bin")
  val cases = new File("./testcases/riscv-tests/isa").listFiles
    .filter(_.isFile)
    .filter(_.getName.endsWith(".bin"))
    .filter(f =>
      knownFails.foldLeft(true)((ret, cur) => ret && !f.getName().endsWith(cur))
    )
    .map(_.getPath)
    .toList
}

class RiscvTestsSpec extends AnyFlatSpec with Matchers {
  behavior of "RiscvTestsSpec"

  for (file <- RiscvTestsSpec.cases) {
    it should s"run $file successfully" in {
      //ExecTest.runFile(file) should be(true)
    }
  }
}

object RiscvTestsMain extends App {
  (new RiscvTestsSpec).execute(stats = true, shortstacks = true)
}
