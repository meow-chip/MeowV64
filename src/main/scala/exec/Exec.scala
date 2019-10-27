package exec
import chisel3._
import reg._
import instr._
import chisel3.util._
import _root_.core.StageCtrl

class BranchResult(val ADDR_WIDTH: Int = 48) extends Bundle {
  val branch = Bool()
  val target = UInt(ADDR_WIDTH.W)
}

class Exec(ADDR_WIDTH: Int) extends Module {
  val io = IO(new Bundle {
    val regReaders = Vec(2, new RegReader)
    val regWriter = new RegWriter
    val instr = Input(new InstrExt(ADDR_WIDTH))

    val ctrl = StageCtrl.stage()

    val branch = Output(new BranchResult(ADDR_WIDTH))
  })

  io.ctrl.stall := false.B
  io.branch := 0.U.asTypeOf(io.branch)
  io.regWriter.addr := 0.U
  io.regWriter.data := 0.U

  val current = Reg(new InstrExt)
  val readRs1 = Reg(UInt(64.W)) // TODO: use XLEN
  val readRs2 = Reg(UInt(64.W))

  printf(p"EX:\n================\n")
  printf(p"Running: ${current}\n\n")
  printf(p"Writing To: 0x${Hexadecimal(io.regWriter.addr)}\n\n")
  printf(p"Writing Data: 0x${Hexadecimal(io.regWriter.data)}\n\n")

  io.regReaders(0).addr := io.instr.instr.rs1
  io.regReaders(1).addr := io.instr.instr.rs2

  when(!io.ctrl.pause) {
    current := io.instr

    readRs1 := io.regReaders(0).data
    readRs2 := io.regReaders(1).data
  }

  switch(current.instr.op) {
    is(Decoder.Op("OP-IMM").ident) {
      io.regWriter.addr := current.instr.rd
      val extended = Wire(SInt(64.W))
      extended := current.instr.imm

      switch(current.instr.funct3) {
        is(Decoder.OP_FUNC("ADD/SUB")) { // Can only be ADDI in OP-IMM
          io.regWriter.data := (extended + readRs1.asSInt).asUInt
        }
        is(Decoder.OP_FUNC("SLL")) {
          io.regWriter.data := (readRs1 << extended(4, 0)).asUInt
        }

        is(Decoder.OP_FUNC("SLT")) {
          when(readRs1.asSInt < extended) {
            io.regWriter.data := 1.U
          }.otherwise {
            io.regWriter.data := 0.U
          }
        }

        is(Decoder.OP_FUNC("SLTU")) {
          when(readRs1 < extended.asUInt) {
            io.regWriter.data := 1.U
          }.otherwise {
            io.regWriter.data := 0.U
          }
        }

        is(Decoder.OP_FUNC("XOR")) {
          io.regWriter.data := extended.asUInt ^ readRs1
        }

        is(Decoder.OP_FUNC("SRL/SRA")) {
          when(current.instr.funct7(5)) {
            // SRA
            io.regWriter.data := (readRs1.asSInt >> extended(4, 0)).asUInt
          }.otherwise {
            // SRL
            io.regWriter.data := readRs1 >> extended(4, 0).asUInt
          }
        }

        is(Decoder.OP_FUNC("OR")) {
          io.regWriter.data := extended.asUInt | readRs1
        }

        is(Decoder.OP_FUNC("AND")) {
          io.regWriter.data := extended.asUInt & readRs1
        }
      }
    }

    is(Decoder.Op("OP-IMM-32").ident) {
      io.regWriter.addr := current.instr.rd
      // First we truncate everything to 32-bit, get a 32-bit result, then
      // sign-extend to 64-bit. TODO: Not sure if it works
      var result32 = Wire(SInt(32.W))
      result32 := 0.S // Default value

      switch(current.instr.funct3) {
        is(Decoder.OP_FUNC("ADD/SUB")) {
          // ADDIW
          result32 = readRs1.asSInt + current.instr.imm
        }

        is(Decoder.OP_FUNC("SLL")) {
          // SLLIW
          // TODO: add assert to check shamt[5]
          result32 = readRs1.asSInt << current.instr.imm(4, 0)
        }

        is(Decoder.OP_FUNC("SRL/SRA")) {
          when(current.instr.funct7(5)) {
            // SRAIW
            result32 = readRs1(31, 0).asSInt >> current.instr.imm(4, 0)
          }.otherwise {
            // SRLIW
            result32 = (readRs1(31, 0).asUInt >> current.instr.imm(4, 0)).asSInt
          }
        }
      }

      val result = Wire(SInt(64.W))
      result := result32
      io.regWriter.data := result.asUInt
    }

    is(Decoder.Op("OP").ident) {
      io.regWriter.addr := current.instr.rd

      switch(current.instr.funct3) {
        is(Decoder.OP_FUNC("ADD/SUB")) {
          when(current.instr.funct7(5)) {
            // Overflows ignored in ADD/SUB
            // SUB
            io.regWriter.data := readRs1 - readRs2
          }.otherwise {
            // ADD
            io.regWriter.data := readRs1 + readRs2
          }
        }

        is(Decoder.OP_FUNC("SLL")) {
          io.regWriter.data := readRs1 << readRs2(5, 0)
        }

        is(Decoder.OP_FUNC("SLT")) {
          when(readRs1.asSInt < readRs2.asSInt) {
            io.regWriter.data := 1.U
          }.otherwise {
            io.regWriter.data := 0.U
          }
        }

        is(Decoder.OP_FUNC("SLTU")) {
          when(readRs1.asUInt < readRs2.asUInt) {
            io.regWriter.data := 1.U
          }.otherwise {
            io.regWriter.data := 0.U
          }
        }

        is(Decoder.OP_FUNC("XOR")) {
          io.regWriter.data := readRs1 ^ readRs2
        }

        is(Decoder.OP_FUNC("SRL/SRA")) {
          when(current.instr.funct7(5)) {
            // SRA
            // In RV64I, only the low 6 bits of rs2 are considered for the
            // shift amount. (c.f. spec p.53)
            io.regWriter.data := (readRs1.asSInt >> readRs2(5, 0)).asUInt
          }.otherwise {
            io.regWriter.data := readRs1 >> readRs2(5, 0)
          }
        }

        is(Decoder.OP_FUNC("OR")) {
          io.regWriter.data := readRs1 | readRs2
        }

        is(Decoder.OP_FUNC("AND")) {
          io.regWriter.data := readRs1 & readRs2
        }
      }
    }

    is(Decoder.Op("OP-32").ident) {
      io.regWriter.addr := current.instr.rd
      val result32 = Wire(UInt(32.W))
      result32 := 0.U // Default value

      switch(current.instr.funct3) {
        is(Decoder.OP_FUNC("ADD/SUB")) {
          when(current.instr.funct7(5)) {
            // SUBW
            result32 := readRs1 - readRs2
          }.otherwise {
            // ADDW
            result32 := readRs1 + readRs2
          }
        }

        is(Decoder.OP_FUNC("SLL")) {
          // SLLW
          result32 := readRs1 << readRs2(4, 0)
        }

        is(Decoder.OP_FUNC("SRL/SRA")) {
          when(current.instr.funct7(5)) {
            // SRAW
            result32 := (readRs1(31, 0).asSInt >> readRs2(4, 0)).asUInt
          }.otherwise {
            // SRLW
            result32 := readRs1(31, 0).asUInt >> readRs2(4, 0)
          }
        }
      }

      // Sign extended
      val result = Wire(SInt(64.W))
      result := result32.asSInt
      io.regWriter.data := result.asUInt
    }

    is(Decoder.Op("LUI").ident) {
      io.regWriter.addr := current.instr.rd
      val extended = Wire(SInt(64.W))
      extended := current.instr.imm
      io.regWriter.data := extended.asUInt
    }

    is(Decoder.Op("AUIPC").ident) {
      io.regWriter.addr := current.instr.rd
      val result = Wire(SInt(64.W))
      result := current.instr.imm + current.addr.asSInt
      io.regWriter.data := result.asUInt
    }
  }
}
