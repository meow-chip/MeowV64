package meowv64.exec.units

import chisel3._
import chisel3.util._
import meowv64.instr.Decoder
import meowv64.exec._
import meowv64.core.CoreDef

class ALUExt(implicit val coredef: CoreDef) extends Bundle {
  val acc = SInt(coredef.XLEN.W)
}

class ALU(override implicit val coredef: CoreDef)
  extends ExecUnit(0, new ALUExt)
{
  def map(stage: Int, pipe: PipeInstr, ext: Option[ALUExt]): (ALUExt, Bool) = {
    val ext = Wire(new ALUExt)
    val acc = Wire(SInt(coredef.XLEN.W))
    acc := DontCare

    val isDWord = (
      pipe.instr.instr.op === Decoder.Op("OP-IMM").ident
      || pipe.instr.instr.op === Decoder.Op("OP").ident
    )

    val op1f = pipe.rs1val.asSInt()
    val op2f = Wire(SInt(coredef.XLEN.W))

    val useSub = Wire(Bool())
    when(pipe.instr.instr.op === Decoder.Op("OP-IMM").ident
      || pipe.instr.instr.op === Decoder.Op("OP-IMM-32").ident) {
        op2f := pipe.instr.instr.imm
        useSub := false.B
      }.otherwise {
        op2f := pipe.rs2val.asSInt()
        useSub := pipe.instr.instr.funct7(5)
      }

    val op1 = Wire(SInt(coredef.XLEN.W))
    val op2 = Wire(SInt(coredef.XLEN.W))
    
    when(isDWord) {
      // Is 64-bit instr
      op1 := op1f.asSInt();
      op2 := op2f.asSInt();
    }.otherwise {
      op1 := op1f(31, 0).asSInt();
      op2 := op2f(31, 0).asSInt();
    }

    switch(pipe.instr.instr.funct3) {
      is(Decoder.OP_FUNC("ADD/SUB")) {
        when(useSub) {
          // Overflows ignored in ADD/SUB
          // SUB
          acc := op1 - op2
        }.otherwise {
          // ADD
          acc := op1 + op2
        }
      }

      is(Decoder.OP_FUNC("SLL")) {
        // TODO: check shamt[5] when HALF = true
        when(isDWord) {
          acc := op1 << op2(5, 0)
        }.otherwise {
          acc := op1 << op2(4, 0)
        }
      }

      is(Decoder.OP_FUNC("SLT")) {
        when(op1 < op2) {
          acc := 1.S
        }.otherwise {
          acc := 0.S
        }
      }

      is(Decoder.OP_FUNC("SLTU")) {
        when(op1.asUInt < op2.asUInt) {
          acc := 1.S
        }.otherwise {
          acc := 0.S
        }
      }

      is(Decoder.OP_FUNC("XOR")) {
        acc := op1 ^ op2
      }

      is(Decoder.OP_FUNC("SRL/SRA")) {
        when(pipe.instr.instr.funct7(5)) {
          // SRA
          // In RV64I, only the low 6 bits of rs2 are considered for the
          // shift amount. (c.f. spec p.53)
          when(isDWord) {
            acc := op1 >> op2(5, 0)
          }.otherwise {
            acc := op1 >> op2(4, 0)
          }
        }.otherwise {
          when(isDWord) {
            acc := (op1.asUInt >> op2(5, 0)).asSInt
          }.otherwise {
            acc := (op1(31, 0).asUInt >> op2(4, 0)).asSInt
          }
        }
      }

      is(Decoder.OP_FUNC("OR")) {
        acc := op1 | op2
      }

      is(Decoder.OP_FUNC("AND")) {
        acc := op1 & op2
      }
    }

    when(isDWord) {
      ext.acc := acc
    }.otherwise {
      ext.acc := acc(31, 0).asSInt // Automatically sign-extends
    }
    (ext, false.B)
  }

  def finalize(pipe: PipeInstr, ext: ALUExt): RetireInfo = {
    // Sign extend if needed
    val info = WireDefault(RetireInfo.vacant)
    val extended = Wire(SInt(coredef.XLEN.W))
    extended := ext.acc
    info.wb := extended.asUInt

    info
  }

  init()
}
