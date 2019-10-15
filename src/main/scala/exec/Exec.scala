package exec
import chisel3._
import reg._
import instr._
import chisel3.util._

class BranchResult(ADDR_WIDTH: Int = 48) extends Bundle {
  val branch = Bool()
  val target = UInt(ADDR_WIDTH.W)
}

class Exec extends Module {
  val io = IO(new Bundle {
    val regReaders = Vec(2, new RegReader)
    val regWriter = new RegWriter
    val instr = Input(new InstrExt)

    val pause = Input(Bool())
    val stall = Output(Bool())

    val branch = Output(new BranchResult)
  })

  io.stall := false.B
  io.branch := 0.U
  io.regWriter.addr := 0.U

  val current = Reg(new InstrExt)
  val readRs1 = Reg(UInt(64.W)) // TODO: use XLEN
  val readRs2 = Reg(UInt(64.W))

  io.regReaders(0).addr := io.instr.instr.rs1
  io.regReaders(1).addr := io.instr.instr.rs2

  when(!io.pause) {
    current := io.instr

    readRs1 := io.regReaders(0).data
    readRs2 := io.regReaders(1).data
  }

  switch(current.instr.op) {
    is(Decoder.Op("OP-IMM").ident) {
      io.regWriter.addr := current.instr.rd
      val extended = SInt(64.W)
      extended := current.instr.imm

      switch(current.instr.funct3) {
        is(Decoder.OP_FUNC("ADD/SUB")) { // Can only be ADDI in OP-IMM
          io.regWriter.data.asSInt := extended + readRs1.asSInt
        }
        is(Decoder.OP_FUNC("SLL")) {
          io.regWriter.data.asSInt := readRs1 << extended(4, 0)
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

    is(Decoder.Op("LUI").ident) {
      io.regWriter.addr := current.instr.rd
      io.regWriter.data.asSInt := current.instr.imm
    }

    is(Decoder.Op("AUIPC").ident) {
      io.regWriter.addr := current.instr.rd
      io.regWriter.data.asSInt := current.instr.imm + current.addr.asSInt
    }
  }
}