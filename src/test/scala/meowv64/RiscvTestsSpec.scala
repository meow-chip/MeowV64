package meowv64

import chiseltest.ChiselScalatestTester
import chiseltest.simulator.WriteVcdAnnotation
import meowv64.multicore.Multicore
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File
import chiseltest.simulator.IcarusBackendAnnotation
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.options.Dependency

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

  it should s"run successfully" in {
    test(
      new Multicore()(ExecDef)
    ).withAnnotations(
      Seq(
        WriteVcdAnnotation,
        IcarusBackendAnnotation,
        RunFirrtlTransformAnnotation(Dependency(ZeroInit)) // for RRArbiter
      )
    ) { dut =>
      for (file <- RiscvTestsSpec.cases) {
        println("------------")
        println(s"Running file $file")
        new ExecTest(dut, file)
      }
    }
  }
}

object RiscvTestsMain extends App {
  (new RiscvTestsSpec).execute(stats = true, shortstacks = true)
}
