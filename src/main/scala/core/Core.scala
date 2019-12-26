package core

import chisel3._
import _root_.data._
import _root_.instr._
import _root_.cache._
import _root_.reg._
import exec.Exec

class Core(val coredef: CoreDef = DefaultDef) extends Module {
  val io = IO(new Bundle {
    val axi = new AXI(coredef.XLEN, coredef.ADDR_WIDTH)
    val eint = Input(Bool())

    // Debug
    val pc = Output(UInt(coredef.XLEN.W))
    val minstret = Output(UInt(coredef.XLEN.W))
    val mcycle = Output(UInt(coredef.XLEN.W))
  })

  assert(coredef.FETCH_NUM % 2 == 0, "issue num can only be multiples of two, because we need to support compressed instructions")

  val ctrl = Module(new Ctrl(coredef))
  
  // Caches
  val l2 = Module(new L2Cache(coredef.L2))
  val l1i = Module(new L1IC(coredef.L1I))
  val l1d = Module(new L1DC(coredef.L1D))

  l2.axi <> io.axi
  l2.ic(0) <> l1i.toL2
  l2.dc(0) <> l1d.toL2

  val fetch = Module(new InstrFetch(coredef))
  val bpu = Module(new BPU(coredef.XLEN, coredef.ADDR_WIDTH, 32, coredef.FETCH_NUM, 2))
  val exec = Module(new Exec()(coredef))
  val reg = Module(new RegFile(coredef.XLEN, 32, coredef.ISSUE_NUM * 2, coredef.RETIRE_NUM))

  val (csrWriter, csr) = CSR.gen(coredef.XLEN, coredef.HART_ID)

  fetch.toCtrl.irst := false.B // For FENCE.I
  fetch.toIC <> l1i.toCPU
  fetch.toCtrl.pc <> ctrl.io.pc
  fetch.toCtrl.skip <> ctrl.io.skip
  fetch.toCtrl.ctrl <> ctrl.io.fetch
  fetch.toCtrl.irst <> ctrl.io.irst

  bpu.toCtrl.upd <> exec.toBPU.isBranch
  bpu.toCtrl.updPC <> exec.toBPU.branchPC
  bpu.toCtrl.fired <> exec.toBPU.branchTaken
  bpu.toFetch.query <> fetch.bpu.query
  bpu.toFetch.pc <> fetch.bpu.pc
  bpu.toFetch.taken <> fetch.bpu.predict

  exec.toIF <> fetch.toExec
  exec.rr <> reg.io.reads
  exec.rw <> reg.io.writes
  exec.toCtrl.ctrl <> ctrl.io.exec
  exec.csrWriter <> csrWriter

  exec.toDC.r <> l1d.r
  exec.toDC.w <> l1d.w
  exec.toDC.fs <> l1d.fs
  exec.toDC.u <> l2.directs(0)
  
  ctrl.br.req <> exec.toCtrl.branch
  ctrl.br.tval <> exec.toCtrl.tval

  ctrl.toExec.retCnt := exec.toCtrl.retCnt
  ctrl.toExec.nepc := exec.toCtrl.nepc
  ctrl.toExec.int <> exec.toCtrl.int
  ctrl.toExec.intAck := exec.toCtrl.intAck

  ctrl.eint := io.eint

  io.pc := ctrl.io.pc

  // CSR
  CSRHelper.defaults(csr)
  csr.const("mhartid") := coredef.HART_ID.U
  csr.attach("mcycle").connect(ctrl.csr.mcycle)
  csr.attach("minstret").connect(ctrl.csr.minstret)
  csr.attach("mstatus").connect(ctrl.csr.mstatus)
  csr.attach("mtvec").connect(ctrl.csr.mtvec)
  csr.attach("mcause").connect(ctrl.csr.mcause)
  csr.attach("mtval").connect(ctrl.csr.mtval)
  csr.attach("mepc").connect(ctrl.csr.mepc)
  csr.attach("mie").connect(ctrl.csr.mie)
  csr.attach("mip").connect(ctrl.csr.mip)
  csr.attach("mcountinhibit").connect(ctrl.csr.mcountinhibit)

  val medeleg = RegInit(0.U(coredef.XLEN.W))
  csr.attach("medeleg").connect(CSRPort.fromReg(coredef.XLEN, medeleg))
  val mideleg = RegInit(0.U(coredef.XLEN.W))
  csr.attach("mideleg").connect(CSRPort.fromReg(coredef.XLEN, mideleg))

  val mscratch = RegInit(0.U(coredef.XLEN.W))
  csr.attach("mscratch").connect(CSRPort.fromReg(coredef.XLEN, mscratch))

  io.mcycle := ctrl.csr.mcycle.rdata
  io.minstret := ctrl.csr.minstret.rdata
}
