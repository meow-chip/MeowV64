package core

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum
import exec.BranchResult

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
  val none, ex, ret = Value
}

class Ctrl(coredef: CoreDef) extends MultiIOModule {
  val io = IO(new Bundle{
    val pc = Output(UInt(coredef.XLEN.W))
    val skip = Output(UInt(log2Ceil(coredef.FETCH_NUM).W))
    val irst = Output(Bool())

    val fetch = StageCtrl.ctrl()
    val exec = StageCtrl.ctrl()
  })

  val br = IO(new Bundle {
    val req = Input(new BranchResult()(coredef))
    val tval = Input(UInt(coredef.XLEN.W))
  })

  val toExec = IO(new Bundle {
    val retCnt = Input(UInt(log2Ceil(coredef.RETIRE_NUM + 1).W))
    val nepc = Input(UInt(coredef.XLEN.W))
    
    val int = Output(Bool())
    val intAck = Input(Bool())
  })

  val eint = IO(Input(Bool()))

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
  });

  val pc = RegInit(coredef.INIT_VEC.U(coredef.XLEN.W))
  val snepc = RegInit(coredef.INIT_VEC.U(coredef.XLEN.W))
  io.pc := pc

  io.fetch.flush := false.B
  io.exec.flush := false.B
  io.irst := false.B

  io.skip := 0.U

  val branch = Wire(Bool())
  val baddr = Wire(UInt(coredef.XLEN.W))

  // MEPC in the front!
  val mepc = RegInit(0.U(coredef.XLEN.W))

  // Next retired instruction
  val nepc = Wire(UInt(coredef.XLEN.W))
  when(toExec.retCnt =/= 0.U) {
    val recv = Wire(UInt(coredef.XLEN.W))
    recv := toExec.nepc
    when(br.req.ex === ExReq.ret) {
      recv := mepc
    }.elsewhen(br.req.branch) {
      recv:= toExec.nepc
    }

    snepc := recv
    nepc := recv
  }.otherwise {
    nepc := snepc
  }

  // Rst comes together with an branch
  // TODO: impl rst (a.k.a. FENCE.I)

  // IF control && PC controllert
  when(branch) {
    // printf(p"Branched, baddr: ${Hexadecimal(io.baddr)}\n")
    io.fetch.flush := true.B
    io.exec.flush := true.B

    assert(!io.fetch.stall)
    assert(!io.exec.stall)

    val instrOffset = log2Ceil(Const.INSTR_MIN_WIDTH / 8)
    val issueOffset = log2Ceil(coredef.FETCH_NUM)
    val pcAlign = instrOffset + issueOffset
    val alignedPC = baddr(coredef.XLEN-1, pcAlign) ## 0.U(pcAlign.W)

    pc := alignedPC + (Const.INSTR_MIN_WIDTH / 8 * coredef.FETCH_NUM).U
    io.pc := alignedPC
    io.skip := baddr(pcAlign, 0) >> instrOffset
    io.irst := br.req.irst

    snepc := baddr
  }.elsewhen(!io.fetch.stall) {
    // printf(p"PC: ${Hexadecimal(io.pc)}\n")
    pc := pc + (Const.INSTR_MIN_WIDTH / 8 * coredef.FETCH_NUM).U
  }

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

  // mstatus
  val mie = RegInit(false.B)
  val mpie = RegInit(false.B)

  // TODO: WPRI?
  csr.mstatus.rdata := (
    0.U(coredef.XLEN)
    | mpie << 7
    | mie << 3
    | 2.U(2.W) << 32 // UXL
  )

  when(csr.mstatus.write) {
    csr.mstatus.wdata := DontCare
    mpie := csr.mstatus.wdata(7)
    mie := csr.mstatus.wdata(3)
  }

  // mie
  val meie = RegInit(false.B)
  val mtie = RegInit(false.B)
  val msie = RegInit(false.B)
  csr.mie.rdata := (
    0.U
    | meie << 11
    | mtie << 7
    | msie << 3
  )

  when(csr.mie.write) {
    meie := csr.mie.wdata(11)
    mtie := csr.mie.wdata(7)
    msie := csr.mie.wdata(3)
  }

  // TODO: wire mip
  csr.mip.rdata := eint
  csr.mip.wdata := DontCare
  csr.mip.write := DontCare

  // mtvec, mtval, mepc, mcause
  val mtvec = RegInit(0.U(coredef.XLEN.W))
  val mtval = RegInit(0.U(coredef.XLEN.W))
  val mcause = RegInit(0.U(coredef.XLEN.W))
  csr.mtvec <> CSRPort.fromReg(coredef.XLEN, mtvec)
  csr.mtval <> CSRPort.fromReg(coredef.XLEN, mtval)
  csr.mepc <> CSRPort.fromReg(coredef.XLEN, mepc)
  csr.mcause <> CSRPort.fromReg(coredef.XLEN, mcause)

  branch := br.req.branch
  baddr := br.req.target

  // Exceptions
  // FIXME: support vercotized mtvec
  toExec.int := eint
  val ex = (br.req.ex === ExReq.ex) || (toExec.intAck && mie)
  val cause = Wire(UInt(coredef.XLEN.W))
  when(toExec.intAck && mie) {
    cause := (true.B << (coredef.XLEN-1)) | 8.U
  }.otherwise {
    cause := (false.B << (coredef.XLEN-1)) | br.req.extype.asUInt()
  }

  // FIXME: interrupt at MRET
  when(ex) {
    // Branch into mtvec
    branch := true.B
    baddr := mtvec(coredef.XLEN-1, 2) ## 0.U(2.W)

    // Save related stuffs
    mepc := nepc
    mcause := cause
    mtval := br.tval

    mpie := mie
    mie := false.B
  }.elsewhen(br.req.ex === ExReq.ret) {
    branch := true.B
    baddr := mepc

    mie := mpie
    mpie := true.B
  }

  // Avoid Vivado naming collision. Com'on, Xilinx, write *CORRECT* code plz
  override def desiredName: String = "PipeCtrl"
}
