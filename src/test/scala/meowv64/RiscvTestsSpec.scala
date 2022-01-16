package meowv64

import chiseltest.ChiselScalatestTester
import meowv64.multicore.Multicore
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

object RiscvTestsSpec {
  // fmadd not implemented
  val knownFails = Seq("rv64ud-p-fmadd.bin", "rv64ud-v-fmadd.bin")
  val cases = new File("./testcases/riscv-tests/build/isa").listFiles
    .filter(_.isFile)
    .filter(_.getName.endsWith(".bin"))
    .filter(f =>
      knownFails.foldLeft(true)((ret, cur) => ret && !f.getName().endsWith(cur))
    )
    .map(_.getPath)
    .sorted
    .toList
}

class RiscvTestsSpec
    extends AnyFlatSpec
    with Matchers
    with ChiselScalatestTester {
  behavior of "RiscvTestsSpec"

  val annotations =
    Simulator.getAnnotations()

  for (
    prefix <- Seq(
      "rv64ui",
      "rv64uc",
      "rv64um",
      "rv64ua",
      "rv64ud",
      "rv64si",
      "rv64mi"
    )
  ) {
    it should s"run ${prefix} testcases successfully" in {
      test(
        new Multicore()(ExecDef)
      ).withAnnotations(annotations) { dut =>
        for (file <- RiscvTestsSpec.cases) {
          if (file.contains(prefix)) {
            println("------------")
            println(s"Running file $file")
            new ExecTest(dut, file)
          }
        }
      }
    }
  }
}

object RiscvTestsMain extends App {
  (new RiscvTestsSpec).execute(stats = true, shortstacks = true)
}
