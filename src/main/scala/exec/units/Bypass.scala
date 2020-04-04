package exec.units

import chisel3._
import chisel3.util._
import instr.Decoder
import exec._
import _root_.core.CoreDef
import _root_.core.ExType
import instr.Decoder.InstrType

class BypassExt(implicit val coredef: CoreDef) extends Bundle {
  val acc = UInt(coredef.XLEN.W)
  val inval = Bool()
}

class Bypass(override implicit val coredef: CoreDef) extends ExecUnit(0, new BypassExt) {
  def map(stage: Int, pipe: PipeInstr, ext: Option[BypassExt]): (BypassExt, chisel3.Bool) = {
    val ext = Wire(new BypassExt)
    ext.acc := DontCare
    ext.inval := true.B

    switch(pipe.instr.instr.op) {
      is(Decoder.Op("LUI").ident) {
        val extended = Wire(SInt(coredef.XLEN.W))
        extended := pipe.instr.instr.imm
        ext.acc := extended.asUInt
        ext.inval := false.B
      }

      is(Decoder.Op("AUIPC").ident) {
        val result = Wire(SInt(coredef.XLEN.W))
        result := pipe.instr.instr.imm + pipe.instr.addr.asSInt
        ext.acc := result.asUInt
        ext.inval := false.B
        // printf(p"AUIPC Written: ${Hexadecimal(result)}\n")
      }

      is(Decoder.Op("JAL").ident) {
        ext.inval := false.B
      }
    }

    (ext, false.B)
  }
  def finalize(pipe: PipeInstr, ext: BypassExt): RetireInfo = {
    val info = WireDefault(RetireInfo.vacant)

    when(pipe.instr.invalAddr) {
      info.branch.ex(ExType.INSTR_ACCESS_FAULT)
      info.wb := pipe.instr.addr
    }.elsewhen(ext.inval) {
      info.branch.ex(ExType.ILLEGAL_INSTR)
      info.wb := 0.U
    }.elsewhen(pipe.instr.instr.op === Decoder.Op("JAL").ident) {
      val linked = Wire(UInt(coredef.VADDR_WIDTH.W))
      linked := pipe.instr.addr + 4.U
      when(pipe.instr.instr.base === InstrType.C) {
        linked := pipe.instr.addr + 2.U // This is an compressed instr
      }

      info.wb := linked

      info.branch.fire((pipe.instr.addr.asSInt + pipe.instr.instr.imm).asUInt)
    }.otherwise {
      info.wb := ext.acc
    }

    info
  }

  init()
}
