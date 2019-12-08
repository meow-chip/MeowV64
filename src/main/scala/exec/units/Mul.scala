package exec.units

import chisel3._
import chisel3.util._
import instr.Decoder
import exec._
import _root_.core.CoreDef

class MulExt(implicit val coredef: CoreDef) extends Bundle {
  val x1 = SInt((coredef.XLEN).W)
  val x2 = SInt((coredef.XLEN).W)
}

class Mul(override implicit val coredef: CoreDef) extends ExecUnit(2, new MulExt) {
  assert(coredef.XLEN == 64)

  override def map(stage: Int, pipe: PipeInstr, _ext: Option[MulExt]): (MulExt, Bool) = {
    val isDWord = (
      pipe.instr.instr.op === Decoder.Op("OP-IMM").ident
      || pipe.instr.instr.op === Decoder.Op("OP").ident
    )

    if(stage == 0) {
      // Pipelining requests
      val ext = Wire(new MulExt)

      val (op1, op2) = (Wire(SInt(coredef.XLEN.W)), Wire(SInt(coredef.XLEN.W)))

      when(isDWord) {
        op1 := pipe.rs1val.asSInt
        op2 := pipe.rs2val.asSInt
      }.otherwise {
        op1 := pipe.rs1val(31, 0).asSInt
        op2 := pipe.rs2val(31, 0).asSInt
      }

      ext.x1 := op1
      ext.x2 := op2
      return (ext, false.B)
    } else if(stage == 1) {
      // printf(p"[MUL  0]: COMP ${Hexadecimal(pipe.rs1val)} * ${Hexadecimal(pipe.rs2val)}\n")
      val prev = _ext.get
      val ext = Wire(new MulExt)
      ext.x2 := DontCare

      when(!isDWord) {
        // Can only be MULW
        ext.x1 := (prev.x1 * prev.x2)(31, 0).asSInt()
      }.otherwise{
        ext.x1 := DontCare
        switch(pipe.instr.instr.funct3) {
          is(Decoder.MULDIV_FUNC("MUL")) {
            ext.x1 := (prev.x1 * prev.x2)(63, 0).asSInt
          }

          is(Decoder.MULDIV_FUNC("MULH")) {
            ext.x1 := (prev.x1 * prev.x2)(127, 64).asSInt
          }

          is(Decoder.MULDIV_FUNC("MULHU")) {
            ext.x1 := (prev.x1.asUInt * prev.x2.asUInt)(127, 64).asSInt
          }

          is(Decoder.MULDIV_FUNC("MULHSU")) {
            ext.x1 := (prev.x1 * prev.x2.asUInt)(127, 64).asSInt
          }
        }
      }

      (ext, false.B)
    } else if(stage == 2) {
      (_ext.get, false.B)
    } else {
      throw new Error(s"Unexpected stage $stage in Mul module")
    }
  }

  override def finalize(pipe: PipeInstr, ext: MulExt): RetireInfo = {
    val info = Wire(new RetireInfo)
    info.branch.nofire()
    info.mem.noop()

    // info.regWaddr := pipe.instr.instr.rd
    info.wb := ext.x1.asUInt

    info
  }

  init()
}
