package core

import chisel3._
import _root_.data._
import _root_.instr._
import _root_.cache._
import _root_.reg._
import exec.Exec

class Core(val coredef: CoreDef = DefaultDef) extends Module {
  val io = IO(new Bundle {
    val iaxi = new AXI(coredef.XLEN, coredef.ADDR_WIDTH)
    val daxi = new AXI(coredef.XLEN, coredef.ADDR_WIDTH)

    // Debug
    val pc = Output(UInt(coredef.ADDR_WIDTH.W))
  })

  assert(coredef.ISSUE_NUM % 2 == 0, "issue num can only be multiples of two, because we need to support compressed instructions")

  val ctrl = Module(new Ctrl(coredef.ADDR_WIDTH, coredef.INIT_VEC, coredef.ISSUE_NUM))

  val ic = Module(new ICache(
    coredef.ADDR_WIDTH,
    Const.INSTR_MIN_WIDTH * coredef.ISSUE_NUM,
    coredef.XLEN
  ))
  val fetch = Module(new InstrFetch(coredef.ADDR_WIDTH, coredef.ISSUE_NUM, coredef.XLEN))
  val exec = Module(new Exec(coredef.ADDR_WIDTH, coredef.XLEN, coredef.ISSUE_NUM))
  val reg = Module(new RegFile(coredef.XLEN))

  fetch.io.icache <> ic.io
  fetch.io.pc <> ctrl.io.pc
  fetch.io.skip <> ctrl.io.skip
  fetch.io.fetch := !ctrl.io.fetch.pause
  fetch.io.ctrl <> ctrl.io.fetch
  fetch.io.axi <> io.iaxi

  // Now we forces FETCH_NUM = 1
  exec.io.instr <> fetch.io.output
  exec.io.regReaders <> reg.io.reads
  exec.io.regWriter <> reg.io.write
  exec.io.ctrl <> ctrl.io.exec
  exec.io.branch <> DontCare
  exec.io.axi <> io.daxi
  
  ctrl.io.branch <> exec.io.branch.branch
  ctrl.io.baddr <> exec.io.branch.target

  io.pc := ctrl.io.pc
}
