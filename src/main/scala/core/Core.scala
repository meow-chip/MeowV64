package core

import chisel3._
import _root_.data._
import _root_.instr._
import _root_.cache._
import _root_.reg._
import exec.Exec

class Core(val coredef: CoreDef = DefaultDef) extends Module {
  val io = IO(new Bundle {
    val axi = new AXI(8, coredef.ADDR_WIDTH)
  })

  val ctrl = Module(new Ctrl(coredef.ADDR_WIDTH, coredef.INIT_VEC, coredef.ISSUE_NUM))

  val ic = Module(new ICache(
    coredef.ADDR_WIDTH,
    32 * coredef.ISSUE_NUM
  ))
  val fetch = Module(new InstrFetch(coredef.ADDR_WIDTH, coredef.ISSUE_NUM))
  val exec = Module(new Exec(coredef.ADDR_WIDTH))
  val reg = Module(new RegFile(coredef.XLEN))

  fetch.io.icache <> ic.io
  fetch.io.pc <> ctrl.io.pc
  fetch.io.fetch := !ctrl.io.fetch.pause
  fetch.io.ctrl <> ctrl.io.fetch
  fetch.io.axi <> io.axi

  // Now we forces FETCH_NUM = 1
  exec.io.instr <> fetch.io.output.asTypeOf(exec.io.instr)
  exec.io.regReaders <> reg.io.reads
  exec.io.regWriter <> reg.io.write
  exec.io.ctrl <> ctrl.io.exec
  exec.io.branch <> DontCare
  
  ctrl.io.branch <> exec.io.branch.branch
  ctrl.io.baddr <> exec.io.branch.target
}
