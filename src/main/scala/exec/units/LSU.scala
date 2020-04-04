package exec.units

import chisel3._
import chisel3.util._
import instr.Decoder
import cache._
import chisel3.experimental.ChiselEnum
import exec._
import _root_.core.CoreDef
import _root_.core.ExType
import _root_.core.ExReq

class LSU(implicit val coredef: CoreDef) extends MultiIOModule with ExecUnitInt with exec.WithLSUPort {
  val DEPTH = 1

  val io = IO(new ExecUnitPort)

  val reader = IO(new DCReader(coredef.L1D))
  val saUp = IO(Output(Bool()))
  val flushed = RegInit(false.B)

  reader := DontCare
  reader.read := false.B
  saUp := false.B

  when(io.flush && reader.stall) {
    flushed := true.B
  }

  when(!reader.stall) {
    flushed := false.B
  }

  // Let's do this without helper

  // Utils
  def isInvalAddr(addr: UInt) = if(coredef.XLEN == coredef.VADDR_WIDTH) { // TODO: physical mode
    false.B
  } else {
    addr(coredef.XLEN-1, coredef.VADDR_WIDTH).orR
  }

  /**
   * Is this access misaligned?
   * 
   * According to ISA, the least two bits of funct3 repersents the size of the load/store:
   * - (L/S)B[U]: 00
   * - (L/S)H[U]: 01
   * - (L/S)W[U]: 10
   * - (L/S)D: 11
   */
  def isMisaligned(offset: UInt, funct3: UInt) = Mux1H(
    UIntToOH(funct3(1, 0)),
    Seq(
      false.B,
      offset(0),
      offset(1, 0).orR,
      offset.orR
    )
  )

  val stall = reader.stall

  /**
   * Stage 1 state
   */ 
  assert(coredef.PADDR_WIDTH > coredef.VADDR_WIDTH)
  val addr = (io.next.rs1val.asSInt + io.next.instr.instr.imm).asUInt
  val aligned = addr(coredef.PADDR_WIDTH - 1, 3) ## 0.U(3.W)
  val offset = addr(2, 0)
  val uncached = addr(coredef.PADDR_WIDTH) // Use bit 56 to denote uncached
  val read = io.next.instr.instr.op === Decoder.Op("LOAD").ident && !io.next.instr.vacant
  val write = io.next.instr.instr.op === Decoder.Op("STORE").ident && !io.next.instr.vacant

  val invalAddr = isInvalAddr(addr)
  val misaligned = isMisaligned(offset, io.next.instr.instr.funct3)

  /**
   * Stage 2 state
   */
  val pipeInstr = RegInit(PipeInstr.empty)
  when(!stall) {
    pipeInstr := io.next
  }
  when(io.flush) {
    pipeInstr.instr.vacant := true.B
  }

  val pipeAddr = (pipeInstr.rs1val.asSInt + pipeInstr.instr.instr.imm).asUInt
  val pipeAligned = pipeAddr(coredef.PADDR_WIDTH - 1, 3) ## 0.U(3.W)
  val pipeOffset = pipeAddr(2, 0)
  val pipeUncached = pipeAddr(coredef.PADDR_WIDTH) // Use bit 56 to denote uncached
  val pipeRead = pipeInstr.instr.instr.op === Decoder.Op("LOAD").ident && !pipeInstr.instr.vacant
  val pipeWrite = pipeInstr.instr.instr.op === Decoder.Op("STORE").ident && !pipeInstr.instr.vacant
  val pipeFenceI = (
    pipeInstr.instr.instr.op === Decoder.Op("MEM-MISC").ident
    && pipeInstr.instr.instr.funct3 === Decoder.MEM_MISC_FUNC("FENCE.I")
    && !pipeInstr.instr.vacant
  )
  val pipeInvalAddr = isInvalAddr(pipeAddr)
  val pipeMisaligned = isMisaligned(pipeOffset, pipeInstr.instr.instr.funct3)

  io.retired := pipeInstr
  io.stall := stall
  when(io.flush) {
    io.retired.instr.vacant := true.B
  }

  // Reader
  reader.addr := aligned
  reader.read := read && !uncached && !invalAddr &&  !misaligned && !io.flush

  val shifted = reader.data >> (pipeOffset << 3) // TODO: use lookup table?
  val signedResult = Wire(SInt(coredef.XLEN.W)).suggestName("signedResult")
  val result = Wire(UInt(coredef.XLEN.W)).suggestName("result")
  shifted.suggestName("shifted")
  result := signedResult.asUInt
  signedResult := DontCare

  switch(pipeInstr.instr.instr.funct3) {
    is(Decoder.LOAD_FUNC("LB")) { signedResult := shifted(7, 0).asSInt }
    is(Decoder.LOAD_FUNC("LH")) { signedResult := shifted(15, 0).asSInt }
    is(Decoder.LOAD_FUNC("LW")) { signedResult := shifted(31, 0).asSInt }
    is(Decoder.LOAD_FUNC("LD")) { result := shifted }
    is(Decoder.LOAD_FUNC("LBU")) { result := shifted(7, 0) }
    is(Decoder.LOAD_FUNC("LHU")) { result := shifted(15, 0) }
    is(Decoder.LOAD_FUNC("LWU")) { result := shifted(31, 0) }
  }

  // Retirement
  when(pipeFenceI) {
    io.retirement.wb := DontCare
    io.retirement.mem.noop()
    io.retirement.branch.ifence(pipeInstr.instr.addr + 4.U)
  }.elsewhen(pipeInvalAddr) {
    io.retirement.wb := DontCare
    io.retirement.mem.noop()
    io.retirement.branch.ex(Mux(
      pipeRead,
      ExType.LOAD_ACCESS_FAULT,
      ExType.STORE_ACCESS_FAULT
    ))
  }.elsewhen(pipeMisaligned) {
    io.retirement.wb := DontCare
    io.retirement.mem.noop()
    io.retirement.branch.ex(Mux(
      pipeRead,
      ExType.LOAD_ADDR_MISALIGN,
      ExType.STORE_ADDR_MISALIGN
    ))
  }.elsewhen(pipeRead && !pipeUncached) {
    io.retirement.branch.nofire()
    io.retirement.mem.noop()
    io.retirement.wb := result
  }.elsewhen(pipeRead) {
    io.retirement.branch.nofire()

    val mem = Wire(new MemSeqAcc)

    mem.op := MemSeqAccOp.ul
    mem.addr := pipeAligned
    mem.offset := pipeOffset
    mem.sext := DontCare
    mem.len := DontCare
    switch(pipeInstr.instr.instr.funct3) {
      is(Decoder.LOAD_FUNC("LB")) {
        mem.sext := true.B
        mem.len := MemSeqAccLen.B
      }
      is(Decoder.LOAD_FUNC("LH")) {
        mem.sext := true.B
        mem.len := MemSeqAccLen.H
      }
      is(Decoder.LOAD_FUNC("LW")) {
        mem.sext := true.B
        mem.len := MemSeqAccLen.W
      }
      is(Decoder.LOAD_FUNC("LD")) {
        mem.sext := false.B
        mem.len := MemSeqAccLen.D
      }
      is(Decoder.LOAD_FUNC("LBU")) {
        mem.sext := false.B
        mem.len := MemSeqAccLen.B
      }
      is(Decoder.LOAD_FUNC("LHU")) {
        mem.sext := false.B
        mem.len := MemSeqAccLen.H
      }
      is(Decoder.LOAD_FUNC("LWU")) {
        mem.sext := false.B
        mem.len := MemSeqAccLen.W
      }
    }

    io.retirement.mem := mem
    io.retirement.wb := DontCare
  }.elsewhen(pipeWrite) {
    io.retirement.branch.nofire()

    val mem = Wire(new MemSeqAcc)

    mem.len := DontCare
    switch(pipeInstr.instr.instr.funct3) {
      is(Decoder.STORE_FUNC("SB")) { mem.len := MemSeqAccLen.B }
      is(Decoder.STORE_FUNC("SH")) { mem.len := MemSeqAccLen.H }
      is(Decoder.STORE_FUNC("SW")) { mem.len := MemSeqAccLen.W }
      is(Decoder.STORE_FUNC("SD")) { mem.len := MemSeqAccLen.D }
    }
    mem.offset := pipeOffset
    // TODO: mask uncached bit
    mem.addr := pipeAligned
    mem.sext := false.B

    when(pipeUncached) {
      mem.op := MemSeqAccOp.us
    } otherwise {
      mem.op := MemSeqAccOp.s
    }

    io.retirement.mem := mem
    io.retirement.wb := pipeInstr.rs2val << (pipeOffset << 3)
  }.otherwise {
    // Inval instr?
    io.retirement.branch.ex(ExType.ILLEGAL_INSTR)
    io.retirement.wb := DontCare
    io.retirement.mem.noop()
  }

  saUp := io.retirement.mem.op =/= MemSeqAccOp.no && !io.retired.instr.vacant
}
