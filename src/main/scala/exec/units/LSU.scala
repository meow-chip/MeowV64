package exec.units

import chisel3._
import chisel3.util._
import instr.Decoder
import cache._
import chisel3.experimental.ChiselEnum
import exec._
import _root_.core.CoreDef
import _root_.core.ExType

class LSUExt(implicit val coredef: CoreDef) extends Bundle {
  val mem = new MemSeqAcc
  val wb = UInt(coredef.XLEN.W)
  val lInvalAddr = Bool()
  val sInvalAddr = Bool()

  val lUnaligned = Bool()
  val sUnaligned = Bool()
}

class LSU(override implicit val coredef: CoreDef) extends ExecUnit(1, new LSUExt) with WithLSUPort {
  val reader = IO(new DCReader(coredef.L1D))
  val saUp = IO(Output(Bool()))
  val flushed = RegInit(false.B)

  reader := DontCare
  reader.read := false.B
  saUp := false.B

  override def map(stage: Int, pipe: PipeInstr, pext: Option[LSUExt]): (LSUExt, Bool) =  {
    val ext = Wire(new LSUExt)

    val addr = (pipe.rs1val.asSInt + pipe.instr.instr.imm).asUInt
    val aligned = addr(coredef.ADDR_WIDTH-1, 3) ## 0.U(3.W)
    val offset = addr(2, 0)
    val isUncached = addr(47)
    val isRead = pipe.instr.instr.op === Decoder.Op("LOAD").ident
    val isWrite = pipe.instr.instr.op === Decoder.Op("STORE").ident

    val isCachedRead = isRead && !isUncached
    val isCachedWrite = isWrite && !isUncached
    val isUncachedRead = isRead && isUncached
    val isUncachedWrite = isWrite && isUncached
    val isInvalAddr = addr(coredef.XLEN-1, coredef.ADDR_WIDTH).orR

    // Is unaligned?
    val isUnaligned = Wire(Bool())
    isUnaligned := false.B
    /**
     * According to ISA, the least two bits of funct3 repersents the size of the load/store:
     * - (L/S)B[U]: 00
     * - (L/S)H[U]: 01
     * - (L/S)W[U]: 10
     * - (L/S)D: 11
     */
    val size = pipe.instr.instr.funct3(1, 0)
    when(size === 1.U) {
      isUnaligned := offset(0)
    }.elsewhen(size === 2.U) {
      isUnaligned := offset(1, 0).orR
    }.elsewhen(size === 3.U) {
      isUnaligned := offset.orR
    }
    // For size = 0, addr is always aligned

    if(stage == 0) {

      // First stage, send LOAD addr
      reader.addr := aligned
      reader.read := isCachedRead && !isInvalAddr &&  !isUnaligned

      // Compute write be
      val tailMask = Wire(UInt((coredef.XLEN/8).W))
      tailMask := 0.U

      ext.mem.len := DontCare
      switch(pipe.instr.instr.funct3) {
        is(Decoder.STORE_FUNC("SB")) { ext.mem.len := MemSeqAccLen.B }
        is(Decoder.STORE_FUNC("SH")) { ext.mem.len := MemSeqAccLen.H }
        is(Decoder.STORE_FUNC("SW")) { ext.mem.len := MemSeqAccLen.W }
        is(Decoder.STORE_FUNC("SD")) { ext.mem.len := MemSeqAccLen.D }
      }
      ext.mem.offset := offset
      ext.mem.addr := aligned
      ext.mem.sext := false.B
      // Memory write data
      ext.wb := pipe.rs2val << (offset << 3)
      ext.mem.op := MemSeqAccOp.no

      when(isUncachedWrite) {
        ext.mem.op := MemSeqAccOp.us
      }.elsewhen(isCachedWrite) {
        ext.mem.op := MemSeqAccOp.s
      }

      ext.lInvalAddr := isRead && isInvalAddr
      ext.sInvalAddr := isWrite && isInvalAddr
      ext.lUnaligned := isRead && isUnaligned
      ext.sUnaligned := isWrite && isUnaligned

      (ext, false.B)
    } else {
      val shifted = reader.data >> (offset << 3)
      val signedResult = Wire(SInt(coredef.XLEN.W)).suggestName("signedResult")
      val result = Wire(UInt(coredef.XLEN.W)).suggestName("result")
      shifted.suggestName("shifted")
      result := signedResult.asUInt
      signedResult := DontCare

      ext := pext.get

      when(isCachedRead) {
        switch(pipe.instr.instr.funct3) {
          is(Decoder.LOAD_FUNC("LB")) { signedResult := shifted(7, 0).asSInt }
          is(Decoder.LOAD_FUNC("LH")) { signedResult := shifted(15, 0).asSInt }
          is(Decoder.LOAD_FUNC("LW")) { signedResult := shifted(31, 0).asSInt }
          is(Decoder.LOAD_FUNC("LD")) { result := shifted }
          is(Decoder.LOAD_FUNC("LBU")) { result := shifted(7, 0) }
          is(Decoder.LOAD_FUNC("LHU")) { result := shifted(15, 0) }
          is(Decoder.LOAD_FUNC("LWU")) { result := shifted(31, 0) }
        }

        ext.wb := result
      }.elsewhen(isUncachedRead) {
        ext.mem.op := MemSeqAccOp.ul
        switch(pipe.instr.instr.funct3) {
          is(Decoder.LOAD_FUNC("LB")) {
            ext.mem.sext := true.B
            ext.mem.len := MemSeqAccLen.B
          }
          is(Decoder.LOAD_FUNC("LH")) {
            ext.mem.sext := true.B
            ext.mem.len := MemSeqAccLen.H
          }
          is(Decoder.LOAD_FUNC("LW")) {
            ext.mem.sext := true.B
            ext.mem.len := MemSeqAccLen.W
          }
          is(Decoder.LOAD_FUNC("LD")) {
            ext.mem.sext := false.B
            ext.mem.len := MemSeqAccLen.D
          }
          is(Decoder.LOAD_FUNC("LBU")) {
            ext.mem.sext := false.B
            ext.mem.len := MemSeqAccLen.B
          }
          is(Decoder.LOAD_FUNC("LHU")) {
            ext.mem.sext := false.B
            ext.mem.len := MemSeqAccLen.H
          }
          is(Decoder.LOAD_FUNC("LWU")) {
            ext.mem.sext := false.B
            ext.mem.len := MemSeqAccLen.W
          }
        }
      }

      // Finally, reuse write-back during misalign rw to write tval
      when(isUnaligned) {
        ext.wb := addr
      }

      (ext, reader.stall)
    }
  }

  override def finalize(pipe: PipeInstr, ext: LSUExt): RetireInfo = {
    val info = Wire(new RetireInfo)

    saUp := ext.mem.op =/= MemSeqAccOp.no

    info.wb := ext.wb
    when(
      pipe.instr.instr.op === Decoder.Op("MEM-MISC").ident
      && pipe.instr.instr.funct3 === Decoder.MEM_MISC_FUNC("FENCE.I")
    ) {
      info.branch.ifence(pipe.instr.addr + 4.U)
      info.mem.noop()
    }.elsewhen(ext.lInvalAddr) {
      info.branch.ex(ExType.LOAD_ACCESS_FAULT)
      info.mem.noop()
    }.elsewhen(ext.sInvalAddr) {
      info.branch.ex(ExType.LOAD_ACCESS_FAULT)
      info.mem.noop()
    }.elsewhen(ext.lUnaligned) {
      info.branch.ex(ExType.LOAD_ADDR_MISALIGN)
      info.mem.noop()
    }.elsewhen(ext.sUnaligned) {
      info.branch.ex(ExType.STORE_ADDR_MISALIGN)
      info.mem.noop()
    }.otherwise {
      info.branch.nofire()
      info.mem := ext.mem

      when(ext.mem.addr(coredef.ADDR_WIDTH-2) === false.B) {
        val splashed = ext.mem.addr.asBools
        val edited = Wire(Vec(coredef.XLEN, Bool()))
        edited := splashed
        edited(coredef.ADDR_WIDTH-1) := false.B
        info.mem.addr := edited.asUInt()
      }
    }

    info
  }

  init()
}

trait WithLSUPort {
  val reader: DCReader
  val saUp: Bool
}
