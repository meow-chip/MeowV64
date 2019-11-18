package exec

import chisel3._
import chisel3.util._
import instr.Decoder

class MulExt(val XLEN: Int) extends Bundle {
  val acc = UInt((XLEN).W)
}

class Mul(ADDR_WIDTH: Int, XLEN: Int) extends ExecUnit(1, new MulExt(XLEN), ADDR_WIDTH, XLEN) {
  assert(XLEN == 64)

  override def map(stage: Int, pipe: PipeInstr, _ext: Option[MulExt]): (MulExt, Bool) = {
    if(stage == 0) {
      // printf(p"[MUL  0]: COMP ${Hexadecimal(pipe.rs1val)} * ${Hexadecimal(pipe.rs2val)}\n")
      val ext = Wire(new MulExt(XLEN))

      val isDWord = (
        pipe.instr.instr.op === Decoder.Op("OP-IMM").ident
        || pipe.instr.instr.op === Decoder.Op("OP").ident
      )

      val (op1, op2) = (Wire(SInt(XLEN.W)), Wire(SInt(XLEN.W)))

      when(isDWord) {
        op1 := pipe.rs1val.asSInt
        op2 := pipe.rs2val.asSInt
      }.otherwise {
        op1 := pipe.rs1val(31, 0).asSInt
        op2 := pipe.rs2val(31, 0).asSInt
      }

      when(!isDWord) {
        // Can only be MULW
        val extended = Wire(SInt(64.W))
        extended := (op1 * op2)(31, 0).asSInt()
        ext.acc := extended.asUInt()
      }.otherwise{
        ext.acc := DontCare
        switch(pipe.instr.instr.funct3) {
          is(Decoder.MULDIV_FUNC("MUL")) {
            ext.acc := (op1 * op2).asUInt()(63, 0)
          }

          is(Decoder.MULDIV_FUNC("MULH")) {
            ext.acc := (op1 * op2).asUInt()(127, 64)
          }

          is(Decoder.MULDIV_FUNC("MULHU")) {
            ext.acc := (op1.asUInt * op2.asUInt).asUInt()(127, 64)
          }

          is(Decoder.MULDIV_FUNC("MULHSU")) {
            ext.acc := (op1 * op2.asUInt).asUInt()(127, 64)
          }
        }
      }

      // Never stalls
      (ext, false.B)
    } else if(stage == 1) {
      (_ext.get, false.B)
    } else {
      throw new Error(s"Unexpected stage $stage in Mul module")
    }
  }

  override def finalize(pipe: PipeInstr, ext: MulExt): RetireInfo = {
    val info = Wire(new RetireInfo(ADDR_WIDTH, XLEN))
    info.branch.nofire()

    info.regWaddr := pipe.instr.instr.rd
    info.regWdata := ext.acc

    info
  }

  init()
}
