package exec

import chisel3._
import chisel3.util._
import instr.Decoder

class MulExt(val XLEN: Int) extends Bundle {
  val acc = UInt((XLEN * 2).W)
}

class Mul(ADDR_WIDTH: Int, XLEN: Int, HALF_WIDTH: Boolean) extends ExecUnit(1, new MulExt(
  if(HALF_WIDTH) XLEN/2
  else XLEN
), ADDR_WIDTH, XLEN) {
  assert(XLEN == 64)

  override def map(stage: Int, pipe: PipeInstr, _ext: Option[MulExt]): (MulExt, Bool) = {
    if(stage == 0) {
      // printf(p"[MUL  0]: COMP ${Hexadecimal(pipe.rs1val)} * ${Hexadecimal(pipe.rs2val)}\n")
      val ext = Wire(new MulExt(
        if(HALF_WIDTH) XLEN/2
        else XLEN
      ))

      val (op1, op2) = if(HALF_WIDTH) {
        (pipe.rs1val(31, 0), pipe.rs2val(31, 0))
      } else {
        (pipe.rs1val, pipe.rs2val)
      }

      if(HALF_WIDTH) {
        // Can only be MULW
        ext.acc := (op1.asSInt * op2.asSInt).asUInt
      } else {
        ext.acc := DontCare
        switch(pipe.instr.instr.funct3) {
          is(Decoder.MULDIV_FUNC("MUL"), Decoder.MULDIV_FUNC("MULH")) {
            ext.acc := (op1.asSInt * op2.asSInt).asUInt
          }

          is(Decoder.MULDIV_FUNC("MULHU")) {
            ext.acc := (op1.asUInt * op2.asUInt).asUInt
          }

          is(Decoder.MULDIV_FUNC("MULHSU")) {
            ext.acc := (op1.asSInt * op2.asUInt).asUInt
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
    if(HALF_WIDTH) {
      val extended = Wire(SInt(XLEN.W))
      extended := ext.acc(31, 0).asSInt
      info.regWdata := extended.asUInt
    } else {
      info.regWdata := ext.acc(XLEN*2-1, XLEN)

      when(pipe.instr.instr.funct3 === Decoder.MULDIV_FUNC("MUL")) {
        info.regWdata := ext.acc(XLEN-1, 0)
      }
    }

    info
  }

  init()
}
