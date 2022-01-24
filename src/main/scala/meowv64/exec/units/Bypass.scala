package meowv64.exec.units

import chisel3._
import chisel3.util._
import meowv64.core.CoreDef
import meowv64.core.ExType
import meowv64.exec._
import meowv64.instr.Decoder
import meowv64.instr.Decoder.InstrType
import meowv64.instr.FetchEx

class BypassExt(implicit val coredef: CoreDef) extends Bundle {
  val acc = UInt(coredef.XLEN.W)
  val illegal = Bool()
}

class Bypass(override implicit val coredef: CoreDef)
    extends ExecUnit(0, new BypassExt) {
  def map(
      stage: Int,
      pipe: PipeInstr,
      ext: Option[BypassExt]
  ): (BypassExt, chisel3.Bool) = {
    val ext = Wire(new BypassExt)
    ext.acc := DontCare
    ext.illegal := ~pipe.instr.instr.info.legal

    switch(pipe.instr.instr.op) {
      is(Decoder.Op("LUI").ident) {
        val extended = Wire(SInt(coredef.XLEN.W))
        extended := pipe.instr.instr.imm
        ext.acc := extended.asUInt
      }

      is(Decoder.Op("AUIPC").ident) {
        val result = Wire(SInt(coredef.XLEN.W))
        result := pipe.instr.instr.imm + pipe.instr.addr.asSInt
        ext.acc := result.asUInt
        // printf(p"AUIPC Written: ${Hexadecimal(result)}\n")
      }
    }

    (ext, false.B)
  }
  def finalize(pipe: PipeInstr, ext: BypassExt): RetireInfo = {
    val info = WireDefault(RetireInfo.vacant)

    val ifAddr = WireDefault(pipe.instr.addr)
    when(pipe.instr.acrossPageEx) {
      ifAddr := pipe.instr.addr +% 2.U
    }

    when(pipe.instr.fetchEx === FetchEx.pageFault) {
      info.branch.ex(ExType.INSTR_PAGE_FAULT)
      info.wb := ifAddr
    }.elsewhen(pipe.instr.fetchEx === FetchEx.invalAddr) {
      info.branch.ex(ExType.INSTR_ACCESS_FAULT)
      info.wb := ifAddr
    }.elsewhen(ext.illegal) {
      info.branch.ex(ExType.ILLEGAL_INSTR)
      info.wb := 0.U
    }.elsewhen(pipe.instr.instr.op === Decoder.Op("JAL").ident) {
      val linked = Wire(UInt(coredef.XLEN.W))
      linked := pipe.instr.addr + 4.U
      when(pipe.instr.instr.base === InstrType.C) {
        linked := pipe.instr.addr + 2.U // This is an compressed instr
      }

      info.wb := linked

      // JAL mispredict is handled in InstrFetch
      info.branchTaken := true.B
      info.branch.nofire
    }.otherwise {
      info.wb := ext.acc
    }

    info
  }

  init()
}
