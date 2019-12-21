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
    val uaxi = new AXI(coredef.XLEN, coredef.ADDR_WIDTH)

    val eint = Input(Bool())

    // Debug
    val pc = Output(UInt(coredef.ADDR_WIDTH.W))
  })

  assert(coredef.FETCH_NUM % 2 == 0, "issue num can only be multiples of two, because we need to support compressed instructions")

  val ctrl = Module(new Ctrl(coredef))
  
  // AXI adapters
  val icpass = Module(new L1ICPass(coredef.L1I))
  val dcpass = Module(new L1DCPass(coredef.L1D))
  val ucpass = Module(new UCPass(coredef.L1D))

  icpass.axi <> io.iaxi
  dcpass.axi <> io.daxi
  ucpass.axi <> io.uaxi

  val fetch = Module(new InstrFetch(coredef))
  val exec = Module(new Exec()(coredef))
  val reg = Module(new RegFile(coredef.XLEN, 32, coredef.ISSUE_NUM * 2, coredef.RETIRE_NUM))

  val (csrWriter, csr) = CSR.gen(coredef.XLEN, coredef.HART_ID)

  fetch.toCtrl.irst := false.B // For FENCE.I
  fetch.toIC <> icpass.toCPU
  fetch.toCtrl.pc <> ctrl.io.pc
  fetch.toCtrl.skip <> ctrl.io.skip
  fetch.toCtrl.ctrl <> ctrl.io.fetch
  fetch.toCtrl.irst <> ctrl.io.irst

  exec.toIF <> fetch.toExec
  exec.rr <> reg.io.reads
  exec.rw <> reg.io.writes
  exec.toCtrl.ctrl <> ctrl.io.exec
  exec.csrWriter <> csrWriter

  exec.toDC.r <> dcpass.r
  exec.toDC.w <> dcpass.w
  exec.toDC.fs <> dcpass.fs

  exec.toDC.u <> ucpass.toCPU
  
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
}
