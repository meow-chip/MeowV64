package core

import chisel3._
import chisel3.util._

class StageCtrl extends Bundle {
  val stall = Input(Bool())
  val flush = Output(Bool())
}

object StageCtrl {
  def ctrl() = new StageCtrl
  def stage() = Flipped(new StageCtrl)
}

class Ctrl(coredef: CoreDef) extends MultiIOModule {
  val io = IO(new Bundle{
    val pc = Output(UInt(coredef.ADDR_WIDTH.W))
    val skip = Output(UInt(log2Ceil(coredef.FETCH_NUM).W))

    val branch = Input(Bool())
    val baddr = Input(UInt(coredef.ADDR_WIDTH.W))

    val fetch = StageCtrl.ctrl()
    val exec = StageCtrl.ctrl()
  })

  val csr = IO(new Bundle {
    val mcycle = new CSRPort(coredef.XLEN)
    val mstatus = new CSRPort(coredef.XLEN)
    val mtvec = new CSRPort(coredef.XLEN)
    val mie = new CSRPort(coredef.XLEN)
    val mip = new CSRPort(coredef.XLEN)
    val mepc = new CSRPort(coredef.XLEN)
    val mcause = new CSRPort(coredef.XLEN)
    val mtval = new CSRPort(coredef.XLEN)

    val mcountinhibit = new CSRPort(coredef.XLEN)
  });

  val pc = RegInit(coredef.INIT_VEC.U(coredef.ADDR_WIDTH.W))
  io.pc := pc

  io.fetch.flush := false.B
  io.exec.flush := false.B

  io.skip := 0.U

  // Rst comes together with an branch
  // TODO: impl rst (a.k.a. FENCE.I)
  val performingBranch = io.branch && !io.exec.stall

  // IF control && PC controllert
  when(performingBranch) {
    // printf(p"Branched, baddr: ${Hexadecimal(io.baddr)}\n")
    io.fetch.flush := true.B
    assert(!io.fetch.stall)

    val instrOffset = log2Ceil(Const.INSTR_MIN_WIDTH / 8)
    val issueOffset = log2Ceil(coredef.FETCH_NUM)
    val pcAlign = instrOffset + issueOffset
    val alignedPC = io.baddr(coredef.ADDR_WIDTH-1, pcAlign) ## 0.U(pcAlign.W)

    pc := alignedPC + (Const.INSTR_MIN_WIDTH / 8 * coredef.FETCH_NUM).U
    io.pc := alignedPC
    io.skip := io.baddr(pcAlign, 0) >> instrOffset
  }.elsewhen(!io.fetch.stall) {
    // printf(p"PC: ${Hexadecimal(io.pc)}\n")
    pc := pc + (Const.INSTR_MIN_WIDTH / 8 * coredef.FETCH_NUM).U
  }

  // Exec ctrl
  io.exec.flush := performingBranch

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
  val mcountinhibit = RegInit(0.U(coredef.XLEN.W))
  when(!mcountinhibit(0)) {
    mcycle := mcycle + 1.U
  }
  csr.mcycle <> CSRPort.fromReg(coredef.XLEN, mcycle)
  csr.mcountinhibit <> CSRPort.fromReg(coredef.XLEN, mcountinhibit)

  // mstatus
  val mie = RegInit(false.B)
  val mpie = RegInit(false.B)

  // TODO: WPRI?
  csr.mstatus.rdata := (
    0.U(coredef.XLEN)
    | mpie << 7
    | mie << 3
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
  csr.mip.rdata := 0.U
  csr.mip.wdata := DontCare
  csr.mip.write := DontCare

  // mtvec, mtval, mepc, mcause
  val mtvec = RegInit(0.U(coredef.XLEN.W))
  val mtval = RegInit(0.U(coredef.XLEN.W))
  val mepc = RegInit(0.U(coredef.XLEN.W))
  val mcause = RegInit(0.U(coredef.XLEN.W))
  csr.mtvec <> CSRPort.fromReg(coredef.XLEN, mtvec)
  csr.mtval <> CSRPort.fromReg(coredef.XLEN, mtval)
  csr.mepc <> CSRPort.fromReg(coredef.XLEN, mepc)
  csr.mcause <> CSRPort.fromReg(coredef.XLEN, mcause)

  // Avoid Vivado naming collision. Com'on, Xilinx, write *CORRECT* code plz
  override def desiredName: String = "PipeCtrl"
}
