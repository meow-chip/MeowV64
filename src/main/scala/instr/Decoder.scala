package instr

import chisel3._
import chisel3.util._
import chisel3.internal.firrtl.Width

/**
 * Instruction Decoder
 * We only supports 32-bit length instructions
 */

object Decoder {
  object InstrType extends Enumeration {
    type InstrType = Value
    val RESERVED, R, I, S, B, U, J, C = Value

    def toInt(x: Value) = x match {
      case RESERVED => 0.U(3.W)
      case R => 1.U(3.W)
      case I => 2.U(3.W)
      case S => 3.U(3.W)
      case B => 4.U(3.W)
      case U => 5.U(3.W)
      case J => 6.U(3.W)
      case C => 7.U(3.W)
    }
  }

  case class OpSpec(ident: UInt, t: UInt)

  object spec {
    def apply(s: String, t: InstrType.InstrType) = OpSpec(Integer.parseInt(s, 2).U(5.W), InstrType.toInt(t))
  }

  val Op: Map[String, OpSpec] = Map(
    "LOAD"      -> spec("00000", InstrType.I),
    // We don't have LOAD-FP @ 00001, as we don't have F extension
    // We don't have custom-0 @ 00010
    "MEM-MISC"  -> spec("00011", InstrType.I), // Sort of
    "OP-IMM"    -> spec("00100", InstrType.I),
    "AUIPC"     -> spec("00101", InstrType.U),
    "OP-IMM-32" -> spec("00110", InstrType.I), // Sort of

    "STORE"     -> spec("01000", InstrType.S),
    // We don't have STORE-FP @ 01001, as we don't have F extension
    // We don't have custom-1 @ 01010
    // We don't have AMO @ 01011, as we don't have A extension
    "OP"        -> spec("01100", InstrType.R),
    "LUI"       -> spec("01101", InstrType.U),
    "OP-32"     -> spec("01110", InstrType.R),

    // 10xxx is for F extension

    "BRANCH"    -> spec("11000", InstrType.B),
    "JALR"      -> spec("11001", InstrType.I),
    "JAL"       -> spec("11011", InstrType.J),
    "SYSTEM"    -> spec("11100", InstrType.I) // Sort of
  )

  val BRANCH_FUNC: Map[String, UInt] = Map(
    "BEQ" -> "000",
    "BNE" -> "001",
    "BLT" -> "100",
    "BGE" -> "101",
    "BLTU" -> "110",
    "BGEU" -> "111"
  ).mapValues(Integer.parseInt(_, 2).U(3.W))

  val LOAD_FUNC: Map[String, UInt] = Map(
    "LB" -> "000",
    "LH" -> "001",
    "LW" -> "010",
    "LD" -> "011",
    "LBU" -> "100",
    "LHU" -> "101",
    "LWU" -> "110"
  ).mapValues(Integer.parseInt(_, 2).U(3.W))

  val STORE_FUNC: Map[String, UInt] = Map(
    "SB" -> "000",
    "SH" -> "001",
    "SW" -> "010",
    "SD" -> "011"
  ).mapValues(Integer.parseInt(_, 2).U(3.W))

  // IMM verison is identical, except that we have no SUBI
  val OP_FUNC: Map[String, UInt] = Map(
    "ADD/SUB" -> "000",
    "SLL" -> "001",
    "SLT" -> "010",
    "SLTU" -> "011",
    "XOR" -> "100",
    "SRL/SRA" -> "101",
    "OR" -> "110",
    "AND" -> "111"
  ).mapValues(Integer.parseInt(_, 2).U(3.W))

  val MEM_MISC_FUNC: Map[String, UInt] = Map(
    "FENCE" -> "000",
    "FENCE.I" -> "001"
  ).mapValues(Integer.parseInt(_, 2).U(3.W))

  val SYSTEM_FUNC: Map[String, UInt] = Map(
    "ECALL/EBREAK" -> "000",
    "CSRRW" -> "001",
    "CSRRS" -> "010",
    "CSRRC" -> "011",
    "CSRRWI" -> "101",
    "CSRRSI" -> "110",
    "CSRRCI" -> "111"
  ).mapValues(Integer.parseInt(_, 2).U(3.W))

  implicit class ConvertToBin(self: String) {
    def asBin = Integer.parseInt(self, 2)
    def asBits(len: Width) = self.asBin.U(len)
  }

  implicit class ConvertToInstr(self: Data) {
    assert(self.getWidth == 32, s"Unexpected decoder input width: ${self.getWidth}")

    def asInstr(): Instr = {
      val result = Wire(new Instr)
      val ui = self.asUInt

      when(!ui.orR()) {
        // Defined invalid instr
        result := DontCare
        result.base := InstrType.toInt(InstrType.RESERVED)
      }.elsewhen(ui(1, 0) =/= "11".asBits(2.W)) {
        result := self.asInstr16()
      }.otherwise {
        result := self.asInstr32()
      }

      result
    }

    def asInstr16(): Instr = {
      val result = Wire(new Instr)
      result := DontCare
      result.base := InstrType.toInt(InstrType.C)

      val ui = self.asUInt

      val rs1t = ui(9, 7) + 8.U
      val rs2t = ui(4, 2) + 8.U

      val rs1e = ui(11, 7)
      val rs2e = ui(6, 2)

      // RD position will differ by instr

      def fail() = {
        result.base := InstrType.toInt(InstrType.RESERVED)
        result.rs1 := DontCare
        result.rs2 := DontCare
        result.rd := DontCare
        result.imm := DontCare
        result.funct3 := DontCare
      }

      switch(ui(1, 0) ## ui(15, 13)) {
        is("00000".asBits(5.W)) { // ADDI4SPN
          result.op := Op("OP-IMM").ident
          result.rd := rs2t
          result.rs1 := 2.U
          result.rs2 := DontCare
          result.imm := (ui(10, 7) ## ui(12, 11) ## ui(5) ## ui(6)).asSInt << 2
          result.funct3 := OP_FUNC("ADD/SUB") // ADDI
        }
        is("00001".asBits(5.W)) { // FLD
          fail()
        }
        is("00010".asBits(5.W)) { // LW
          result.op := Op("LOAD").ident
          result.rs1 := rs1t
          result.rs2 := DontCare
          result.rd := rs2t
          result.imm := (ui(5) ## ui(12, 10) ## ui(6) ## 0.U(2.W)).asSInt
          result.funct3 := LOAD_FUNC("LW")
        }
        is("00011".asBits(5.W)) { // LD
          result.op := Op("LOAD").ident
          result.rs1 := rs1t
          result.rs2 := DontCare
          result.rd := rs2t
          result.imm := (ui(6, 5) ## ui(12, 10) ## 0.U(3.W)).asSInt
          result.funct3 := LOAD_FUNC("LD")
        }
        is("00100".asBits(5.W)) { // Reserved
          fail()
        }
        is("00101".asBits(5.W)) { // FSD
          fail()
        }
        is("00110".asBits(5.W)) { // SW
          result.op := Op("STORE").ident
          result.rs1 := rs1t
          result.rs2 := rs2t
          result.rd := DontCare
          result.imm := (ui(5) ## ui(12, 10) ## ui(6) ## 0.U(2.W)).asSInt
          result.funct3 := STORE_FUNC("SW")
        }
        is("00111".asBits(5.W)) { // SD
          result.op := Op("STORE").ident
          result.rs1 := rs1t
          result.rs2 := rs2t
          result.rd := DontCare
          result.imm := (ui(6, 5) ## ui(12, 10) ## 0.U(2.W)).asSInt
          result.funct3 := STORE_FUNC("SD")
        }

        is("01000".asBits(5.W)) { // ADDI
          result.op := Op("OP-IMM").ident
          result.rd := rs1e
          result.rs1 := rs1e
          result.rs2 := DontCare
          result.imm := (ui(12) ## ui(6, 2)).asSInt
          result.funct3 := OP_FUNC("ADD/SUB") // Can only be ADDI
        }
        is("01001".asBits(5.W)) { // ADDIW
          result.op := Op("OP-IMM-32").ident
          result.rd := rs1e
          result.rs1 := rs1e
          result.rs2 := DontCare
          result.imm := (ui(12) ## ui(6, 2)).asSInt
          result.funct3 := OP_FUNC("ADD/SUB") // Can only be ADDI
        }
        is("01010".asBits(5.W)) { // LI, encode as ori rd, x0, imm
          result.op := Op("OP-IMM").ident
          result.rd := rs1e
          result.rs1 := 0.U
          result.rs2 := DontCare
          result.imm := (ui(12) ## ui(6, 2)).asSInt
          result.funct3 := OP_FUNC("OR")
        }
        is("01011".asBits(5.W)) { // LUI/ADDI16SP
          when(rs1e === 2.U) { // ADDI16SP
            result.op := Op("OP-IMM").ident
            result.rd := 2.U
            result.rs1 := 2.U
            result.rs2 := DontCare
            result.imm := (ui(12) ## ui(4, 3) ## ui(5) ## ui(2) ## ui(6)).asSInt << 4
            result.funct3 := OP_FUNC("ADD/SUB") // ADDI
          }.otherwise { // LUI
            result.op := Op("LUI").ident
            result.rd := rs1e
            result.rs1 := DontCare
            result.rs2 := DontCare
            result.imm := (ui(12) ## ui(6, 2)).asSInt << 12
            result.funct3 := DontCare
          }
        }
        is("01100".asBits(5.W)) { // MISC-ALU
          when(ui(11) === 0.U) { // SRLI64 / SRAI64
            result.op := Op("OP-IMM").ident
            result.rs1 := rs1t
            result.rs2 := DontCare
            result.rd := rs1t
            result.imm := (0.U(1.W) ## ui(12) ## ui(6, 2)).asSInt
            result.funct3 := OP_FUNC("SRL/SRA")
            result.funct7 := ui(11, 10) ## 0.U(5.U)
          }.elsewhen(ui(10) === 0.U) { // ANDI
            result.op := Op("OP-IMM").ident
            result.rs1 := rs1t
            result.rs2 := DontCare
            result.rd := rs1t
            result.imm := (ui(12) ## ui(6, 2)).asSInt
            result.funct3 := OP_FUNC("AND")
          }.otherwise { // OP MISC
            result.op := Op("OP").ident
            result.rs1 := rs1t
            result.rs2 := rs2t
            result.rd := rs1t
            result.imm := DontCare

            switch(ui(12) ## ui(6, 5)) {
              is("000".asBits(3.W)) { // SUB
                result.funct3 := OP_FUNC("ADD/SUB")
                result.funct7 := "0100000".asBits(7.W)
              }

              is("001".asBits(3.W)) { // XOR
                result.funct3 := OP_FUNC("XOR")
              }

              is("010".asBits(3.W)) { // OR
                result.funct3 := OP_FUNC("OR")
              }

              is("011".asBits(3.W)) { // AND
                result.funct3 := OP_FUNC("AND")
              }

              is("100".asBits(3.W)) { // SUBW
                result.op := Op("OP-32").ident
                result.funct3 := OP_FUNC("ADD/SUB")
                result.funct7 := "0100000".asBits(7.W)
              }

              is("101".asBits(3.W)) { // ADDW
                result.op := Op("OP-32").ident
                result.funct3 := OP_FUNC("ADD/SUB")
                result.funct7 := "0000000".asBits(7.W)
              }

              is("110".asBits(3.W)) { // Reserved
                fail()
              }

              is("111".asBits(3.W)) { // Reserved
                fail()
              }
            }
          }
        }
        is("01101".asBits(5.W)) { // J
          result.op := Op("JAL").ident
          result.rs1 := DontCare
          result.rs2 := DontCare
          result.rd := 0.U // Ignore result
          result.imm := (
            ui(12) ## ui(8) ## ui(10, 9) ## ui(6) ## ui(7) ## ui(2) ## ui(11) ## ui(5, 3) ## 0.U(1.W)
          ).asSInt
          result.funct3 := DontCare // TODO: trigger error on treadle when reading DontCare
        }
        is("01110".asBits(5.W)) { // BEQZ
          result.op := Op("BRANCH").ident
          result.rs1 := rs1t
          result.rs2 := 0.U // Compare with zero
          result.rd := DontCare
          result.imm := (ui(12) ## ui(6, 5) ## ui(2) ## ui(11, 10) ## ui(4, 3) ## 0.U(1.W)).asSInt
          result.funct3 := BRANCH_FUNC("BEQ")
        }
        is("01111".asBits(5.W)) { // BNEZ
          result.op := Op("BRANCH").ident
          result.rs1 := rs1t
          result.rs2 := 0.U // Compare with zero
          result.rd := DontCare
          result.imm := (ui(12) ## ui(6, 5) ## ui(2) ## ui(11, 10) ## ui(4, 3) ## 0.U(1.W)).asSInt
          result.funct3 := BRANCH_FUNC("BNE")
        }

        is("10000".asBits(5.W)) { // SLLI
          result.op := Op("OP-IMM").ident
          result.rd := rs1e
          result.rs1 := rs1e
          result.rs2 := DontCare
          result.imm := (0.U(1.W) ## ui(12) ## ui(6, 2)).asSInt
          result.funct3 := OP_FUNC("SLL")
        }
        is("10001".asBits(5.W)) { // FLDSP
          fail()
        }
        is("10010".asBits(5.W)) { // LWSP
          result.op := Op("LOAD").ident
          result.rs1 := 2.U // x2 = sp
          result.rs2 := DontCare
          result.rd := rs1e
          result.imm := (ui(3, 2) ## ui(12) ## ui(6, 4) ## 0.U(2.W)).asSInt
          result.funct3 := LOAD_FUNC("LW")
        }
        is("10011".asBits(5.W)) { // LDSP
          result.op := Op("LOAD").ident
          result.rs1 := 2.U // x2 = sp
          result.rs2 := DontCare
          result.rd := rs1e
          result.imm := (ui(4, 2) ## ui(12) ## ui(6, 3) ## 0.U(3.W)).asSInt
          result.funct3 := LOAD_FUNC("LD")
        }
        is("10100".asBits(5.W)) { // J[AL]R/MV/ADD
          when(ui(12) === 0.U) {
            when(rs2e === 0.U) { // JR
              result.op := Op("JALR").ident
              result.rs1 := rs1e
              result.rs2 := DontCare
              result.rd := 0.U // Ignore result
              result.imm := 0.S
              result.funct3 := DontCare
            }.otherwise { // MV, encode as or rd, x0, rs2, same as add rd, x0, rs2
              result.op := Op("OP").ident
              result.rd := rs1e
              result.rs1 := 0.U
              result.rs2 := rs2e
              result.imm := DontCare
              result.funct3 := OP_FUNC("OR")
            }
            // TODO: EBREAK, maybe?
          }.otherwise {
            when(rs2e === 0.U) { // JALR
              result.op := Op("JALR").ident
              result.rs1 := rs1e
              result.rs2 := DontCare
              result.rd := 1.U // x1 = ra
              result.imm := 0.S
              result.funct3 := DontCare
            }.otherwise { // ADD
              result.op := Op("OP").ident
              result.rs1 := rs1e
              result.rs2 := rs2e
              result.rd := rs1e
              result.imm := DontCare
              result.funct3 := OP_FUNC("ADD/SUB")
              result.funct7 := "0100000".asBits(7.W)
            }
          }
        }
        is("10101".asBits(5.W)) { // FSDSP
          fail()
        }
        is("10110".asBits(5.W)) { // SWSP
          result.op := Op("STORE").ident
          result.rs1 := 2.U // x2 = sp
          result.rs2 := rs2e
          result.rd := DontCare
          result.imm := (ui(8, 7) ## ui(12, 9) ## 0.U(2.W)).asSInt
          result.funct3 := STORE_FUNC("SW")
        }
        is("10111".asBits(5.W)) { // SDSP
          result.op := Op("STORE").ident
          result.rs1 := 2.U // x2 = sp
          result.rs2 := rs2e
          result.rd := DontCare
          result.imm := (ui(9, 7) ## ui(12, 10) ## 0.U(3.W)).asSInt
          result.funct3 := STORE_FUNC("SD")
        }
      }

      result
    }

    def asInstr32(): Instr = {
      val ui = self.asUInt
      val result = Wire(new Instr)
      result.op := ui >> 2

      var ctx: Option[WhenContext] = None

      for((op, OpSpec(pat, typ)) <- Op) {
        ctx = Some(ctx match {
          case Some(c) => c.elsewhen(result.op === pat) { result.base := typ }
          case None => when(result.op === pat) { result.base := typ }
        })
      }

      ctx.get.otherwise { result.base := InstrType.toInt(InstrType.RESERVED) }

      // Really parse the instr
      result.funct3 := ui(14, 12)
      result.funct7 := ui(31, 25)
      result.rd := ui(11, 7)
      result.rs1 := ui(19, 15)
      result.rs2 := ui(24, 20)

      // Parse immediate
      result.imm := 0.S
      when(result.base === InstrType.toInt(InstrType.I)) {
        result.imm := ui(31, 20).asSInt
      }.elsewhen(result.base === InstrType.toInt(InstrType.S)) {
        result.imm := (ui(31, 25) ## ui(11, 7)).asSInt
      }.elsewhen(result.base === InstrType.toInt(InstrType.B)) {
        result.imm := (ui(31) ## ui(7) ## ui(30, 25) ## ui(11, 8) ## 0.U(1.W)).asSInt
      }.elsewhen(result.base === InstrType.toInt(InstrType.U)) {
        result.imm := ui(31, 12).asSInt << 12
      }.elsewhen(result.base === InstrType.toInt(InstrType.J)) {
        result.imm := (ui(31) ## ui(19, 12) ## ui(20) ## ui(30, 21) ## 0.U(1.W)).asSInt
      }

      result
    }
  }
}

class Instr extends Bundle {
  // Opcode
  val op = UInt(5.W)
  val base = UInt(3.W)

  // Immediates
  val imm = SInt(32.W)

  // Registers
  val rs1 = UInt(5.W)
  val rs2 = UInt(5.W)
  val rd = UInt(5.W)

  // Funct
  val funct7 = UInt(7.W)
  val funct3 = UInt(3.W)

  override def toPrintable: Printable = {
    // Reverse check op
    p"Instr: \n" +
    p"  Base: ${Hexadecimal(base)}\n" +
    p"  Op:   ${Hexadecimal(op)}\n" +
    p"  Imm:  ${Hexadecimal(imm)}\n" +
    p"  RS1:  ${Hexadecimal(rs1)}\n" +
    p"  RS2:  ${Hexadecimal(rs2)}\n" +
    p"  RD:   ${Hexadecimal(rd)}\n" +
    p"  F7:   ${Hexadecimal(funct7)}\n" +
    p"  F3:   ${Hexadecimal(funct3)}"
  }
}
