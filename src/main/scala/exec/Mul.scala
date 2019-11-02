package exec

import chisel3._
import chisel3.util._
import instr.Decoder

class MulExt(val XLEN: Int) extends Bundle {
  val acc = UInt((XLEN * 2).W)
}

class Mul(ADDR_WIDTH: Int, XLEN: Int, USE_IMM: Boolean) extends ExecUnit(1, new MulExt(XLEN), ADDR_WIDTH, XLEN) {
  assert(XLEN == 64)

  override def map(stage: Int, pipe: PipeInstr, _ext: Option[MulExt]): (MulExt, Bool) = {
    if(stage == 0) {
      val ext = Wire(new MulExt(XLEN))
      switch(pipe.instr.instr.funct3) {
        is(Decoder.MULDIV_FUNC("MUL"), Decoder.MULDIV_FUNC("MULH")) {
          val op1 = pipe.rs1val.asSInt()
          val op2 = if(USE_IMM) { pipe.instr.instr.imm } else { pipe.rs2val.asSInt() }

          ext.acc := (op1 * op2).asUInt
        }

        is(Decoder.MULDIV_FUNC("MULHU")) {
          val op1 = pipe.rs1val
          val op2 = if(USE_IMM) { pipe.instr.instr.imm.asUInt } else { pipe.rs2val }

          ext.acc := (op1 * op2).asUInt
        }

        is(Decoder.MULDIV_FUNC("MULHSU")) {
          val op1 = pipe.rs1val.asSInt()
          val op2 = if(USE_IMM) { pipe.instr.instr.imm } else { pipe.rs2val.asSInt() }

          ext.acc := (op1 * op2).asUInt
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
    info.regWdata := ext.acc(XLEN*2-1, XLEN)

    when(pipe.instr.instr.funct3 === Decoder.MULDIV_FUNC("MUL")) {
      info.regWdata := ext.acc(XLEN-1, 0)
    }

    info
  }
}
