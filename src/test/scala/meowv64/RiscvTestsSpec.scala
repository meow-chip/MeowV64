package meowv64

import chiseltest.ChiselScalatestTester
import meowv64.multicore.Multicore
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

object RiscvTestsSpec {
  val knownFails = Seq("rv64mi-p-scall.bin")
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

  it should s"run physical testcases successfully" in {
    test(
      new Multicore()(ExecDef)
    ).withAnnotations(annotations) { dut =>
      for (file <- RiscvTestsSpec.cases) {
        if (file.contains("-p-")) {
          println("------------")
          println(s"Running file $file")
          new ExecTest(dut, file)
        }
      }
    }
  }

  // these testcases are slow and memory consuming
  // avoid java heap space oom
  for (file <- RiscvTestsSpec.cases) {
    if (file.contains("-v-")) {
      it should s"run testcases ${file} successfully" in {
        test(
          new Multicore()(ExecDef)
        ).withAnnotations(annotations) { dut =>
          println("------------")
          println(s"Running file $file")
          new ExecTest(dut, file)
        }
      }
    }
  }
}

object RiscvTestsMain extends App {
  (new RiscvTestsSpec).execute(stats = true, shortstacks = true)
}
