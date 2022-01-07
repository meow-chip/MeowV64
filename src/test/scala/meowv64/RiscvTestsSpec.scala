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
    .toList
}

class RiscvTestsSpec
    extends AnyFlatSpec
    with Matchers
    with ChiselScalatestTester {
  behavior of "RiscvTestsSpec"

  it should s"run successfully" in {
    for (file <- RiscvTestsSpec.cases) {
      if (file.contains("sll")) {
        println(s"------------\nRunning file $file")
        test(
          new Multicore()(ExecDef)
        ).withAnnotations(
          Seq(
            WriteVcdAnnotation,
            IcarusBackendAnnotation,
            RunFirrtlTransformAnnotation(Dependency(ZeroInit)) // for RRArbiter
          )
        ) {
          new ExecTest(_, file)
        }
      }
    }
  }
}

object RiscvTestsMain extends App {
  (new RiscvTestsSpec).execute(stats = true, shortstacks = true)
}
