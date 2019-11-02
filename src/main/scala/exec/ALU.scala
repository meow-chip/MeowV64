package exec

import chisel3._
import chisel3.util._
import instr.Decoder

class ALUExt(val XLEN: Int) extends Bundle {
  val acc = SInt(XLEN.W)
}

class ALU(ADDR_WIDTH: Int, XLEN: Int, HALF_SIZE: Boolean)
  extends ExecUnit(0, new ALUExt(if(HALF_SIZE) XLEN / 2 else XLEN), ADDR_WIDTH, XLEN)
{
  val VAL_SIZE = if(HALF_SIZE) XLEN / 2 else XLEN

  def map(stage: Int, pipe: PipeInstr, ext: Option[ALUExt]): (ALUExt, Bool) = {
    val ext = Wire(new ALUExt(VAL_SIZE))
    ext.acc := DontCare
    val op1f = pipe.rs1val.asSInt()
    val op2f = Wire(SInt(XLEN.W))

    val useSub = Wire(Bool())
    when(pipe.instr.instr.op === Decoder.Op("OP-IMM").ident
      || pipe.instr.instr.op === Decoder.Op("OP-IMM-32").ident) {
        op2f := pipe.instr.instr.imm
        useSub := false.B
      }.otherwise {
        op2f := pipe.rs2val.asSInt()
        useSub := pipe.instr.instr.funct7(5)
      }
    val (op1, op2) = if(HALF_SIZE) (op1f(31, 0).asSInt, op2f(31, 0).asSInt) else (op1f, op2f)

    switch(pipe.instr.instr.funct3) {
      is(Decoder.OP_FUNC("ADD/SUB")) {
        when(useSub) {
          // Overflows ignored in ADD/SUB
          // SUB
          ext.acc := op1 - op2
        }.otherwise {
          // ADD
          ext.acc := op1 + op2
        }
      }

      is(Decoder.OP_FUNC("SLL")) {
        // TODO: check shamt[5] when HALF = true
        ext.acc := op1 << op2(5, 0)
      }

      is(Decoder.OP_FUNC("SLT")) {
        when(op1 < op2) {
          ext.acc := 1.S
        }.otherwise {
          ext.acc := 0.S
        }
      }

      is(Decoder.OP_FUNC("SLTU")) {
        when(op1.asUInt < op2.asUInt) {
          ext.acc := 1.S
        }.otherwise {
          ext.acc := 0.S
        }
      }

      is(Decoder.OP_FUNC("XOR")) {
        ext.acc := op1 ^ op2
      }

      is(Decoder.OP_FUNC("SRL/SRA")) {
        when(pipe.instr.instr.funct7(5)) {
          // SRA
          // In RV64I, only the low 6 bits of rs2 are considered for the
          // shift amount. (c.f. spec p.53)
          ext.acc := op1 >> op2(5, 0)
        }.otherwise {
          ext.acc := (op1.asUInt >> op2(5, 0)).asSInt
        }
      }

      is(Decoder.OP_FUNC("OR")) {
        ext.acc := op1 | op2
      }

      is(Decoder.OP_FUNC("AND")) {
        ext.acc := op1 & op2
      }
    }

    (ext, false.B)
  }

  def finalize(pipe: PipeInstr, ext: ALUExt): RetireInfo = {
    // Sign extend if needed
    val info = Wire(new RetireInfo(ADDR_WIDTH, XLEN))
    info.branch.nofire()
    info.regWaddr := pipe.instr.instr.rd

    val extended = Wire(SInt(XLEN.W))
    extended := ext.acc
    info.regWdata := extended.asUInt

    info
  }

  init()
}
