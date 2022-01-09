package meowv64.core

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import meowv64.exec.BranchResult

class StageCtrl extends Bundle {
  val stall = Input(Bool())
  val flush = Output(Bool())
}

object StageCtrl {
  def ctrl() = new StageCtrl
  def stage() = Flipped(new StageCtrl)
}

object ExType extends ChiselEnum {
  val INSTR_ADDR_MISALIGN = Value(0.U)
  val INSTR_ACCESS_FAULT = Value(1.U)
  val ILLEGAL_INSTR = Value(2.U)
  val BREAKPOINT = Value(3.U)
  val LOAD_ADDR_MISALIGN = Value(4.U)
  val LOAD_ACCESS_FAULT = Value(5.U)
  val STORE_ADDR_MISALIGN = Value(6.U)
  val STORE_ACCESS_FAULT = Value(7.U)
  val U_CALL = Value(8.U)
  val S_CALL = Value(9.U)
  val M_CALL = Value(11.U)
  val INSTR_PAGE_FAULT = Value(12.U)
  val LOAD_PAGE_FAULT = Value(13.U)
  val STORE_PAGE_FAULT = Value(15.U)
}

object ExReq extends ChiselEnum {
  val none, ex, mret, sret = Value
}

object PrivLevel extends ChiselEnum {
  val U = Value(0.U)
  val S = Value(1.U)
  val H = Value(2.U) // Although we don't support H...
  val M = Value(3.U)
}

class Ctrl(implicit coredef: CoreDef) extends Module {
  val toIF = IO(new Bundle {
    val ctrl = StageCtrl.ctrl()

    val pc = Output(UInt(coredef.XLEN.W))
    val irst = Output(Bool())
    val tlbrst = Output(Bool())

    val priv = Output(PrivLevel())
  })

  val br = IO(new Bundle {
    val req = Input(new BranchResult()(coredef))
    val tval = Input(UInt(coredef.XLEN.W))
  })

  val toExec = IO(new Bundle {
    val ctrl = StageCtrl.ctrl()

    val retCnt = Input(UInt(log2Ceil(coredef.RETIRE_NUM + 1).W))
    val nepc = Input(UInt(coredef.XLEN.W))

    val int = Output(Bool())
    val intAck = Input(Bool())

    val priv = Output(PrivLevel())
    val status = Output(new Status)

    val tlbrst = Output(Bool())
  })

  val int = IO(Input(new CoreInt))

  val csr = IO(new Bundle {
    val mcycle = new CSRPort(coredef.XLEN)
    val minstret = new CSRPort(coredef.XLEN)
    val mstatus = new CSRPort(coredef.XLEN)
    val mtvec = new CSRPort(coredef.XLEN)
    val mie = new CSRPort(coredef.XLEN)
    val mip = new CSRPort(coredef.XLEN)
    val mepc = new CSRPort(coredef.XLEN)
    val mcause = new CSRPort(coredef.XLEN)
    val mtval = new CSRPort(coredef.XLEN)
    val mcountinhibit = new CSRPort(coredef.XLEN)
    val mideleg = new CSRPort(coredef.XLEN)
    val medeleg = new CSRPort(coredef.XLEN)

    val sstatus = new CSRPort(coredef.XLEN)
    val stvec = new CSRPort(coredef.XLEN)
    val sie = new CSRPort(coredef.XLEN)
    val sip = new CSRPort(coredef.XLEN)
    val scause = new CSRPort(coredef.XLEN)
    val sepc = new CSRPort(coredef.XLEN)
    val stval = new CSRPort(coredef.XLEN)

    val instret = Output(UInt(64.W))
    val cycle = Output(UInt(64.W))
  });

  // Privilege level
  val priv = RegInit(PrivLevel.M);
  toExec.priv := priv;

  toIF.ctrl.flush := false.B
  toExec.ctrl.flush := false.B
  toExec.tlbrst := false.B
  toIF.irst := DontCare
  toIF.tlbrst := false.B
  toIF.pc := DontCare
  toIF.priv := priv

  val branch = Wire(Bool())
  val baddr = Wire(UInt(coredef.XLEN.W))

  // xEPC in the front!
  val mepc = RegInit(0.U(coredef.XLEN.W))
  val sepc = RegInit(0.U(coredef.XLEN.W))

  // Next retired instruction
  val nepc = Wire(UInt(coredef.XLEN.W))
  when(br.req.ex === ExReq.mret) {
    nepc := mepc
  }.elsewhen(br.req.ex === ExReq.sret) {
    nepc := sepc
  }.otherwise {
    nepc := toExec.nepc
  }

  // Rst comes together with an branch

  // IF control && PC controller
  when(branch) {
    // printf(p"Branched, baddr: ${Hexadecimal(io.baddr)}\n")
    toIF.ctrl.flush := true.B
    toExec.ctrl.flush := true.B

    assert(!toIF.ctrl.stall)
    assert(!toExec.ctrl.stall)

    // pc := alignedPC + (Const.INSTR_MIN_WIDTH / 8 * coredef.FETCH_NUM).U
    toIF.pc := baddr
    toIF.irst := br.req.irst
    toIF.tlbrst := br.req.tlbrst

    toExec.tlbrst := br.req.tlbrst
  }
  /*
    // printf(p"PC: ${Hexadecimal(io.pc)}\n")
    pc := pc + (Const.INSTR_MIN_WIDTH / 8 * coredef.FETCH_NUM).U
  }
   */

  /*
  printf("Ctrl status:\n")
  printf("================\n")
  printf(p"PC: ${pc}\n")
  printf(p"Stalled: ${stalled}\n")
  when(io.fetch.stall) {
    printf("  Fetch stall")
  }
  when(io.exec.stall) {
    printf("  Exec stall")
  }
  printf("\n")
   */

  val mcycle = RegInit(0.U(coredef.XLEN.W))
  val minstret = RegInit(0.U(coredef.XLEN.W))
  val mcountinhibit = RegInit(0.U(coredef.XLEN.W))

  when(!mcountinhibit(0)) {
    mcycle := mcycle + 1.U
  }

  when(!mcountinhibit(1)) {
    minstret := minstret + toExec.retCnt
  }

  csr.mcycle <> CSRPort.fromReg(coredef.XLEN, mcycle)
  csr.minstret <> CSRPort.fromReg(coredef.XLEN, minstret)
  csr.mcountinhibit <> CSRPort.fromReg(coredef.XLEN, mcountinhibit)

  csr.instret := minstret
  csr.cycle := mcycle

  // xstatus
  val status = RegInit(Status.empty)
  val mwpri = RegInit(0.U(coredef.XLEN.W))
  val swpri = RegInit(0.U(coredef.XLEN.W))
  toExec.status := status

  csr.mstatus.rdata := (
    status.asUInt & Status.mmask
      | mwpri & Status.mwpri
      | Status.hardwired.asUInt & ~(Status.mmask | Status.mwpri)
  )

  csr.sstatus.rdata := (
    status.asUInt & Status.smask
      | swpri & Status.swpri
      | Status.hardwired.asUInt & ~(Status.smask | Status.swpri)
  )

  when(csr.mstatus.write) {
    status := (
      csr.mstatus.wdata & Status.mmask | status.asUInt & ~Status.mmask
    ).asTypeOf(status)
    mwpri := csr.mstatus.wdata
  }

  when(csr.sstatus.write) {
    status := (
      csr.sstatus.wdata & Status.smask | status.asUInt & ~Status.smask
    ).asTypeOf(status)
    swpri := csr.sstatus.wdata
  }

  // MxDELEG
  val medeleg = RegInit(0.U(coredef.XLEN.W))
  val mideleg = RegInit(0.U(coredef.XLEN.W))
  csr.medeleg <> CSRPort.fromReg(coredef.XLEN, medeleg)
  csr.mideleg <> CSRPort.fromReg(coredef.XLEN, mideleg)

  // xIE, xIP
  val ipStore = RegInit(IntConf.empty)
  val ie = RegInit(IntConf.empty)
  val ip = WireDefault(ipStore)
  ip.external.m := int.meip
  ip.timer.m := int.mtip
  ip.software.m := int.msip
  ip.external.s := int.seip
  // TODO: CSRW SEIP

  val miewpri = RegInit(0.U(coredef.XLEN.W))
  val mipwpri = RegInit(0.U(coredef.XLEN.W))
  val siewpri = RegInit(0.U(coredef.XLEN.W))
  val sipwpri = RegInit(0.U(coredef.XLEN.W))

  csr.mie.rdata := (
    ie.asUInt & IntConf.mmask(false)
      | mwpri & IntConf.mwpri
      | IntConf.hardwired.asUInt & ~(Status.mmask | Status.mwpri)
  )

  csr.sie.rdata := (
    ie.asUInt & IntConf.smask(false)
      | swpri & IntConf.swpri
      | IntConf.hardwired.asUInt & ~(Status.smask | Status.swpri)
  )

  when(csr.mie.write) {
    ie := (
      csr.mie.wdata & IntConf.mmask(false) | ie.asUInt & ~Status.mmask
    ).asTypeOf(ie)
    mwpri := csr.mie.wdata
  }

  when(csr.sie.write) {
    ie := (
      csr.sie.wdata & IntConf.smask(false) | ie.asUInt & ~Status.smask
    ).asTypeOf(ie)
    swpri := csr.sie.wdata
  }

  csr.mip.rdata := (
    ip.asUInt & IntConf.mmask(false)
      | mwpri & IntConf.mwpri
      | IntConf.hardwired.asUInt & ~(Status.mmask | Status.mwpri)
  )

  csr.sip.rdata := (
    ip.asUInt & IntConf.smask(false)
      | swpri & IntConf.swpri
      | IntConf.hardwired.asUInt & ~(Status.smask | Status.swpri)
  )

  when(csr.mip.write) {
    ipStore := (
      csr.mip.wdata & IntConf.mmask(false) | ipStore.asUInt & ~Status.mmask
    ).asTypeOf(ipStore)
    mwpri := csr.mip.wdata
  }

  when(csr.sip.write) {
    ipStore := (
      csr.sip.wdata & IntConf.smask(false) | ipStore.asUInt & ~Status.smask
    ).asTypeOf(ipStore)
    swpri := csr.sip.wdata
  }

  // xEPC
  csr.mepc <> CSRPort.fromReg(coredef.XLEN, mepc)
  csr.sepc <> CSRPort.fromReg(coredef.XLEN, sepc)

  // xtvec, xtval, xcause
  val mtvec = RegInit(0.U(coredef.XLEN.W))
  val mtval = RegInit(0.U(coredef.XLEN.W))
  val mcause = RegInit(0.U(coredef.XLEN.W))
  val stvec = RegInit(0.U(coredef.XLEN.W))
  val stval = RegInit(0.U(coredef.XLEN.W))
  val scause = RegInit(0.U(coredef.XLEN.W))
  csr.mtvec <> CSRPort.fromReg(coredef.XLEN, mtvec)
  csr.mtval <> CSRPort.fromReg(coredef.XLEN, mtval)
  csr.mcause <> CSRPort.fromReg(coredef.XLEN, mcause)
  csr.stvec <> CSRPort.fromReg(coredef.XLEN, stvec)
  csr.stval <> CSRPort.fromReg(coredef.XLEN, stval)
  csr.scause <> CSRPort.fromReg(coredef.XLEN, scause)

  branch := br.req.branch
  baddr := br.req.target

  // Interrupts
  val intMask: UInt = (ie.asUInt & ip.asUInt())
  val intCause = PriorityEncoder(intMask)
  val intDeleg = priv =/= PrivLevel.M && mideleg(intCause)
  val intEnabled = Mux(
    intDeleg,
    priv < PrivLevel.S || status.sie,
    priv < PrivLevel.M || status.mie
  )
  val intFired = intEnabled && intMask.asUInt().orR()
  toExec.int := intFired

  // Exceptions + Interrupts
  val ex = (br.req.ex === ExReq.ex) || (toExec.intAck && intFired)
  val cause = Wire(UInt(coredef.XLEN.W))
  when(intFired) {
    cause := (true.B << (coredef.XLEN - 1)) | intCause
  }.otherwise {
    cause := (false.B << (coredef.XLEN - 1)) | br.req.extype.asUInt()
  }

  // Exception delegated to S-mode
  val delegs = WireDefault(
    Mux(
      intFired,
      intDeleg,
      priv =/= PrivLevel.M && medeleg(cause(62, 0))
    )
  )

  when(priv === PrivLevel.M) {
    // When we are already in M-mode, don't delegate
  }

  val tvecBase = Mux(delegs, stvec, mtvec)
  val tvec = Wire(UInt(coredef.XLEN.W))
  tvec := tvecBase(coredef.XLEN - 1, 2) ## 0.U(2.W)
  when(intFired && tvecBase(1, 0) === 1.U) {
    // Vectored trap
    tvec := (tvecBase(coredef.XLEN - 1, 2) + intCause) ## 0.U(2.W)
  }

  // FIXME: interrupt at MRET
  when(ex) {
    // Branch into mtvec

    branch := true.B
    baddr := tvec

    when(delegs) {
      sepc := nepc
      scause := cause
      stval := br.tval

      status.spie := status.sie
      status.sie := false.B
      status.spp := priv =/= PrivLevel.U

      priv := PrivLevel.S
    } otherwise {
      mepc := nepc
      mcause := cause
      mtval := br.tval

      status.mpie := status.mie
      status.mie := false.B
      status.mpp := priv.asUInt()

      priv := PrivLevel.M
    }
  }.elsewhen(br.req.ex === ExReq.mret) {
    branch := true.B
    baddr := mepc

    status.mie := status.mpie
    status.mpie := true.B
    status.mpp := PrivLevel.U.asUInt()

    priv := status.mpp.asTypeOf(PrivLevel.Type())
  }.elsewhen(br.req.ex === ExReq.sret) {
    branch := true.B
    baddr := sepc

    status.sie := status.spie
    status.spie := true.B
    status.spp := PrivLevel.U.asUInt()

    priv := status.spp.asTypeOf(PrivLevel.Type())
  }

  // Avoid Vivado naming collision. Com'on, Xilinx, write *CORRECT* code plz
  override def desiredName: String = "PipeCtrl"
}
