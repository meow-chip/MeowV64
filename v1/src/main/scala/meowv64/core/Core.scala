package meowv64.core

import chisel3._
import meowv64.cache._
import meowv64.exec.Exec
import meowv64.instr._
import meowv64.paging.PTW
import meowv64.reg._

class CoreInt extends Bundle {
  val meip = Bool()
  val seip = Bool()
  val mtip = Bool()
  val msip = Bool()
}

class CoreFrontend(implicit val coredef: CoreDef) extends Bundle {
  val ic = new L1ICPort(coredef.L1I)
  val dc = new L1DCPort(coredef.L1D)
  val uc = new L1UCPort(coredef.L1D)
}

class CoreDebug(implicit val coredef: CoreDef) extends Bundle {
  val pc = UInt(coredef.XLEN.W)
  val minstret = UInt(coredef.XLEN.W)
  val mcycle = UInt(coredef.XLEN.W)
}

class Core(implicit val coredef: CoreDef) extends Module {
  val io = IO(new Bundle {
    val int = Input(new CoreInt)
    val frontend = new CoreFrontend
    val time = Input(UInt(64.W))

    // Debug
    val debug = Output(new CoreDebug)
  })

  assert(
    coredef.FETCH_NUM % 2 == 0,
    "issue num can only be multiples of two, because we need to support compressed instructions"
  )

  val ctrl = Module(new Ctrl)

  // Caches
  val l1i = Module(new L1IC(coredef.L1I))
  val l1d = Module(new L1DC(coredef.L1D))

  l1i.toL2 <> io.frontend.ic
  l1d.toL2 <> io.frontend.dc

  // TODO: attach DTLB
  val ptw = Module(new PTW)
  l1d.ptw <> ptw.dc

  val fetch = Module(new InstrFetch)
  val bpu = Module(new BPU)
  val ras = Module(new RAS)
  val exec = Module(new Exec)
  val regFiles = for ((ty, width) <- coredef.REGISTER_TYPES) yield {
    val reg = Module(
      new RegFile(
        width,
        32,
        coredef.ISSUE_NUM * 3, // rs1-3
        coredef.RETIRE_NUM,
        // hardwire x0 to zero
        FIXED_ZERO = (ty == RegType.integer)
      )
    )
    reg.suggestName(s"reg_${ty}")
  }

  val (csrWriter, csr) = CSR.gen(coredef.XLEN, coredef.HART_ID)

  fetch.toIC <> l1i.toCPU
  fetch.toCtrl <> ctrl.toIF
  fetch.toCore.ptw <> ptw.itlb

  bpu.toExec <> exec.toBPU
  bpu.toFetch <> fetch.toBPU

  ras.toIF <> fetch.toRAS
  ras.toExec.realign.bits := DontCare
  ras.toExec.realign.valid := false.B

  exec.toIF <> fetch.toExec
  for (i <- 0 until coredef.REGISTER_TYPES.length) {
    exec.toRF.ports(i).rr <> regFiles(i).io.reads
    exec.toRF.ports(i).rw <> regFiles(i).io.writes
  }
  exec.csrWriter <> csrWriter

  exec.toDC.r <> l1d.mr
  exec.toDC.w <> l1d.w
  exec.toDC.fs <> l1d.fs
  exec.toDC.u <> io.frontend.uc

  exec.toCtrl.ctrl <> ctrl.toExec.ctrl
  exec.toCtrl.tlbRst := ctrl.toExec.tlbRst

  exec.toCore.ptw <> ptw.dtlb

  ctrl.br.req <> exec.toCtrl.branch
  ctrl.br.tval <> exec.toCtrl.tval

  ctrl.toExec.retCnt := exec.toCtrl.retCnt
  ctrl.toExec.nepc := exec.toCtrl.nepc
  ctrl.toExec.int <> exec.toCtrl.int
  ctrl.toExec.intAck := exec.toCtrl.intAck
  ctrl.toExec.priv <> exec.toCtrl.priv
  ctrl.toExec.status <> exec.toCtrl.status
  ctrl.toExec.fflags <> exec.toCtrl.fflags

  ctrl.int := io.int

  io.debug.pc := fetch.debug.pc

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
  csr.attach("mideleg").connect(ctrl.csr.mideleg)
  csr.attach("medeleg").connect(ctrl.csr.medeleg)

  csr.attach("sstatus").connect(ctrl.csr.sstatus)
  csr.attach("stvec").connect(ctrl.csr.stvec)
  csr.attach("sie").connect(ctrl.csr.sie)
  csr.attach("sip").connect(ctrl.csr.sip)
  csr.attach("scause").connect(ctrl.csr.scause)
  csr.attach("sepc").connect(ctrl.csr.sepc)
  csr.attach("stval").connect(ctrl.csr.stval)

  csr.attach("fflags").connect(ctrl.csr.fflags)
  csr.attach("frm").connect(ctrl.csr.frm)
  csr.attach("fcsr").connect(ctrl.csr.fcsr)

  val mscratch = RegInit(0.U(coredef.XLEN.W))
  csr.attach("mscratch").connect(CSRPort.fromReg(coredef.XLEN, mscratch))
  val sscratch = RegInit(0.U(coredef.XLEN.W))
  csr.attach("sscratch").connect(CSRPort.fromReg(coredef.XLEN, sscratch))

  val satp = RegInit(Satp.empty)
  csr.attach("satp").connect(satp.port)

  csr.readers("cycle") := ctrl.csr.cycle
  csr.readers("time") := io.time
  csr.readers("instret") := ctrl.csr.instret

  ptw.satp := satp
  fetch.toCore.satp := satp
  exec.toCore.satp := satp

  io.debug.mcycle := ctrl.csr.mcycle.rdata
  io.debug.minstret := ctrl.csr.minstret.rdata
}
