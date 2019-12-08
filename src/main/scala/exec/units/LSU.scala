package exec.units

import chisel3._
import chisel3.util._
import instr.Decoder
import cache._
import chisel3.experimental.ChiselEnum
import exec._
import _root_.core.CoreDef

class LSUExt(implicit val coredef: CoreDef) extends Bundle {
  val mem = new MemSeqAcc
  val wb = UInt(coredef.XLEN.W)
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
    val isCachedRead = pipe.instr.instr.op === Decoder.Op("LOAD").ident && !isUncached
    val isCachedWrite = pipe.instr.instr.op === Decoder.Op("STORE").ident && !isUncached
    val isUncachedRead = pipe.instr.instr.op === Decoder.Op("LOAD").ident && isUncached
    val isUncachedWrite = pipe.instr.instr.op === Decoder.Op("STORE").ident && isUncached

    if(stage == 0) {

      // First stage, send LOAD addr
      reader.addr := aligned
      reader.read := isCachedRead

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

      (ext, reader.stall)
    }
  }

  override def finalize(pipe: PipeInstr, ext: LSUExt): RetireInfo = {
    val info = Wire(new RetireInfo)
    info.branch.nofire()
    info.wb := ext.wb
    info.mem := ext.mem

    saUp := ext.mem.op =/= MemSeqAccOp.no

    info
  }

  init()
}

trait WithLSUPort {
  val reader: DCReader
  val saUp: Bool
}
