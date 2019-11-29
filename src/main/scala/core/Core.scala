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

  assert(coredef.FETCH_NUM % 2 == 0, "issue num can only be multiples of two, because we need to support compressed instructions")

  val ctrl = Module(new Ctrl(coredef.ADDR_WIDTH, coredef.INIT_VEC, coredef.FETCH_NUM))
  
  // Caches
  val l2 = Module(new L2Cache(coredef.L2))
  val l1i = Module(new L1IC(coredef.L1I))

  l2.axi <> io.iaxi
  l2.ic <> VecInit(Seq(l1i.toL2))
  l2.dc(0) <> L1DCPort.empty(coredef.L1D)
  l2.directs(0) <> L1DCPort.empty(coredef.L1D)

  val fetch = Module(new InstrFetch(coredef))
  val exec = Module(new Exec(coredef))
  val reg = Module(new RegFile(coredef.XLEN))

  val (csrWriter, csr, counterPort) = CSR.gen(coredef.XLEN, coredef.HART_ID)
  val mcycle = Module(new Counter(coredef.XLEN))
  counterPort <> mcycle.io

  fetch.toCtrl.irst := false.B // For FENCE.I
  fetch.toIC <> l1i.toCPU
  fetch.toCtrl.pc <> ctrl.io.pc
  fetch.toCtrl.skip <> ctrl.io.skip
  fetch.toCtrl.ctrl <> ctrl.io.fetch

  exec.toIF <> fetch.toExec
  exec.io.regReaders <> reg.io.reads
  exec.io.regWriter <> reg.io.write
  exec.io.ctrl <> ctrl.io.exec
  exec.io.axi <> io.daxi
  exec.io.csrWriter <> csrWriter
  
  ctrl.io.branch <> exec.io.branch.branch
  ctrl.io.baddr <> exec.io.branch.target

  io.pc := ctrl.io.pc
}
