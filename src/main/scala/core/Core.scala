package core

import chisel3._
import _root_.data._
import _root_.instr._
import _root_.cache._
import _root_.reg._
import exec.Exec

class Core extends Module {
  val io = IO(new Bundle {
    val axi = new AXI(8)
  })

  val ctrl = Module(new Ctrl(Def.ADDR_WIDTH, Def.INIT_VEC, Def.ISSUE_NUM))

  val ic = Module(new ICache(
    Def.ADDR_WIDTH,
    32 * Def.ISSUE_NUM
  ))
  val fetch = Module(new InstrFetch(Def.ADDR_WIDTH, Def.ISSUE_NUM))
  val exec = Module(new Exec(Def.ADDR_WIDTH))
  val reg = Module(new RegFile(Def.XLEN))

  fetch.io.icache <> ic.io
  fetch.io.pc <> ctrl.pc
  fetch.io.fetch := !ctrl.io.fetch.pause
  fetch.io.ctrl <> ctrl.io.fetch

  // Now we forces FETCH_NUM = 1
  exec.io.instr <> fetch.io.output.asTypeOf(exec.io.instr)
  exec.io.regReaders <> reg.io.reads
  exec.io.regWriter <> reg.io.write
  exec.io.ctrl <> ctrl.io.exec
  exec.io.branch <> DontCare
}