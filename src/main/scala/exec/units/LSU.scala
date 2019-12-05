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

  reader := DontCare
  reader.read := false.B

  override def map(stage: Int, pipe: PipeInstr, pext: Option[LSUExt]): (LSUExt, Bool) =  {
    val ext = Wire(new LSUExt)

    // TODO: handle flush

    val addr = (pipe.rs1val.asSInt + pipe.instr.instr.imm).asUInt
    val aligned = addr(coredef.ADDR_WIDTH-1, 3) ## 0.U(3.W)
    val offset = addr(2, 0)
    val isUncached = false.B
    val isCachedRead = pipe.instr.instr.op === Decoder.Op("LOAD").ident && !isUncached
    val isCachedWrite = pipe.instr.instr.op === Decoder.Op("STORE").ident && !isUncached

    if(stage == 0) {

      // First stage, send LOAD addr
      reader.addr := aligned
      reader.read := isCachedRead

      // Compute write be
      val tailMask = Wire(UInt((coredef.XLEN/8).W))
      tailMask := 0.U

      switch(pipe.instr.instr.funct3) {
        is(Decoder.STORE_FUNC("SB")) { tailMask := 0x01.U }
        is(Decoder.STORE_FUNC("SH")) { tailMask := 0x03.U }
        is(Decoder.STORE_FUNC("SW")) { tailMask := 0x0f.U }
        is(Decoder.STORE_FUNC("SD")) { tailMask := 0xff.U }
      }
      ext.mem.be := tailMask << offset
      ext.mem.addr := aligned
      // Memory write data
      ext.wb := pipe.rs2val << (offset << 3)
      ext.mem.op := MemSeqAccOp.no

      when(isCachedWrite) {
        ext.mem.op := MemSeqAccOp.s
      }

      (ext, reader.stall)
    } else {
      val shifted = reader.data >> (offset << 3)
      val signedResult = Wire(SInt(coredef.XLEN.W)).suggestName("signedResult")
      val result = Wire(UInt(coredef.XLEN.W)).suggestName("result")
      shifted.suggestName("shifted")
      result := signedResult.asUInt
      signedResult := DontCare

      switch(pipe.instr.instr.funct3) {
        is(Decoder.LOAD_FUNC("LB")) { signedResult := shifted(7, 0).asSInt }
        is(Decoder.LOAD_FUNC("LH")) { signedResult := shifted(15, 0).asSInt }
        is(Decoder.LOAD_FUNC("LW")) { signedResult := shifted(31, 0).asSInt }
        is(Decoder.LOAD_FUNC("LD")) { result := shifted }
        is(Decoder.LOAD_FUNC("LBU")) { result := shifted(7, 0) }
        is(Decoder.LOAD_FUNC("LHU")) { result := shifted(16, 0) }
        is(Decoder.LOAD_FUNC("LWU")) { result := shifted(32, 0) }
      }

      ext := pext.get
      when(isCachedRead) {
        ext.wb := result
      }

      (ext, false.B)
    }
  }

  override def finalize(pipe: PipeInstr, ext: LSUExt): RetireInfo = {
    val info = Wire(new RetireInfo)
    info.branch.nofire()
    info.wb := ext.wb
    info.mem := ext.mem

    info
  }

  init()
}

trait WithLSUPort {
  val reader: DCReader
}
