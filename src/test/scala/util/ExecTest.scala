package util

import core._
import chisel3._
import chisel3.iotesters.PeekPokeTester

object ExecDef extends CoreDef {
  override val INIT_VEC = BigInt(0)
}

class WrappedCore(coredef: CoreDef, ifile: String) extends Module {
  val io = IO(new Bundle {
    val ended = Output(Bool())
  })

  val core = Module(new Core(coredef))

  val mem = Module(new AXIMem(Some(ifile), 65536, core.coredef.ADDR_WIDTH, 64, Some(BigInt(0xFFFF10000000L))))

  core.io.axi <> mem.io.axi

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
      case None => chisel3.iotesters.Driver(
        () => new WrappedCore(ExecDef, file),
        "verilator"
      ) { new ExecTest(_) }

      case Some(args) => chisel3.iotesters.Driver.execute(
        args,
        () => new WrappedCore(ExecDef, file)
      ) { new ExecTest(_) }
    }
  }
}
