package meowv64.instr

import chisel3._
import chisel3.experimental._
import chisel3.internal.firrtl.Width
import chisel3.util._
import meowv64.instr.Decoder.InstrType
import meowv64.reg.RegType

/** Instruction Decoder We only supports 32-bit length instructions
  */

object Decoder {
  object InstrType extends ChiselEnum {
    val RESERVED = Value

    /** funct7, rs2, rs1, funct3, rd, opcode */
    val R = Value

    /** imm[11:0], rs1, funct3, rd, opcode */
    val I = Value

    /** imm[11:5], rs2, rs1, funct3, imm[4:0], opcode */
    val S = Value

    /** imm[12|10:5], rs2, rs1, funct3, imm[4:1|11], opcode */
    val B = Value

    /** imm[31:12], rd, opcode */
    val U = Value

    /** imm[20|10:1|11|19:12], rd, opcode */
    val J = Value
    val C = Value

    /** rs3, funct2, rs2, rs1, funct3, rd, opcode */
    val R4 = Value
  }

  case class OpSpec(ident: UInt, t: InstrType.Type)

  object spec {
    def apply(s: String, t: InstrType.Type) =
      OpSpec(Integer.parseInt(s, 2).U(5.W), t)
  }

  /** Instruction op types(inst[6:2]), see RISC-V base opcode map
    */
  val Op: Map[String, OpSpec] = Map(
    "LOAD" -> spec("00000", InstrType.I),
    "LOAD-FP" -> spec("00001", InstrType.I),
    // We don't have custom-0 @ 00010
    "MISC-MEM" -> spec("00011", InstrType.I), // Sort of
    "OP-IMM" -> spec("00100", InstrType.I),
    "AUIPC" -> spec("00101", InstrType.U),
    "OP-IMM-32" -> spec("00110", InstrType.I), // Sort of

    "STORE" -> spec("01000", InstrType.S),
    "STORE-FP" -> spec("01001", InstrType.S),
    // We don't have custom-1 @ 01010
    "AMO" -> spec("01011", InstrType.R),
    "OP" -> spec("01100", InstrType.R),
    "LUI" -> spec("01101", InstrType.U),
    "OP-32" -> spec("01110", InstrType.R),

    // 10xxx is for F extension
    "MADD" -> spec("10000", InstrType.R4),
    "MSUB" -> spec("10001", InstrType.R4),
    "NMSUB" -> spec("10010", InstrType.R4),
    "NMADD" -> spec("10011", InstrType.R4),
    "OP-FP" -> spec("10100", InstrType.R),
    "BRANCH" -> spec("11000", InstrType.B),
    "JALR" -> spec("11001", InstrType.I),
    "JAL" -> spec("11011", InstrType.J),
    "SYSTEM" -> spec("11100", InstrType.I) // Sort of
  )

  val BRANCH_FUNC: Map[String, UInt] = Map(
    "BEQ" -> "000",
    "BNE" -> "001",
    "BLT" -> "100",
    "BGE" -> "101",
    "BLTU" -> "110",
    "BGEU" -> "111"
  ).mapValues(Integer.parseInt(_, 2).U(3.W))

  val MEM_WIDTH_FUNC: Map[String, UInt] = Map(
    "B" -> "000",
    "H" -> "001",
    "W" -> "010",
    "D" -> "011",
    "BU" -> "100",
    "HU" -> "101",
    "WU" -> "110"
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
    "PRIV" -> "000",
    "CSRRW" -> "001",
    "CSRRS" -> "010",
    "CSRRC" -> "011",
    "CSRRWI" -> "101",
    "CSRRSI" -> "110",
    "CSRRCI" -> "111"
  ).mapValues(Integer.parseInt(_, 2).U(3.W))

  val PRIV_FUNCT7: Map[String, UInt] = Map(
    "SFENCE.VMA" -> "0001001"
  ).mapValues(Integer.parseInt(_, 2).U(7.W))

  val PRIV_RS2: Map[String, UInt] = Map(
    "ECALL" -> "00000",
    "EBREAK" -> "00001",
    "RET" -> "00010",
    "WFI" -> "00101"
  ).mapValues(Integer.parseInt(_, 2).U(5.W))

  val MULDIV_FUNCT7: UInt = Integer.parseInt("0000001", 2).U(7.W)
  val MULDIV_FUNC: Map[String, UInt] = Map(
    "MUL" -> "000",
    "MULH" -> "001",
    "MULHSU" -> "010",
    "MULHU" -> "011",
    "DIV" -> "100",
    "DIVU" -> "101",
    "REM" -> "110",
    "REMU" -> "111"
  ).mapValues(Integer.parseInt(_, 2).U(3.W))

  val AMO_FUNC: Map[String, UInt] = Map(
    "LR" -> "00010",
    "SC" -> "00011",
    "AMOSWAP" -> "00001",
    "AMOADD" -> "00000",
    "AMOXOR" -> "00100",
    "AMOAND" -> "01100",
    "AMOOR" -> "01000",
    "AMOMIN" -> "10000",
    "AMOMAX" -> "10100",
    "AMOMINU" -> "11000",
    "AMOMAXU" -> "11100"
  ).mapValues(Integer.parseInt(_, 2).U(5.W))

  val FP_FUNC: Map[String, UInt] = Map(
    "FADD" -> "00000",
    "FSUB" -> "00001",
    "FMUL" -> "00010",
    "FSGNJ" -> "00100",
    "FMINMAX" -> "00101",
    "FLOAT2FLOAT" -> "01000",
    "FSQRT" -> "01011",
    "FCMP" -> "10100",
    "FLOAT2INT" -> "11000",
    "INT2FLOAT" -> "11010",
    "FMV.X.D/W" -> "11100",
    "FCLASS" -> "11100",
    "FMV.D/W.X" -> "11110"
  ).mapValues(Integer.parseInt(_, 2).U(5.W))

  implicit class ConvertToBin(self: String) {
    def asBin = Integer.parseInt(self, 2)
    def asBits(len: Width) = self.asBin.U(len)
  }

  implicit class ConvertToInstr(self: Data) {
    def parseInstr(): (Instr, Bool) = {
      assert(
        self.getWidth == 32,
        s"Unexpected decoder input width: ${self.getWidth}"
      )
      val result = Wire(new Instr)
      val isInstr16 = WireDefault(false.B)
      val ui = self.asUInt

      when(!ui.orR()) {
        // Defined invalid instr
        result := DontCare
        result.base := InstrType.RESERVED
        result.info := DecodeInfo.invalid
      }.elsewhen(ui(1, 0) =/= "11".asBits(2.W)) {
        result := self.asInstr16()
        isInstr16 := true.B
      }.otherwise {
        result := self.asInstr32()
      }

      (result, isInstr16)
    }

    def asInstr16(): Instr = {
      val result = Wire(new Instr)
      result := DontCare
      result.base := InstrType.C

      val ui = self.asUInt
      result.raw := ui

      // 0-7 -> 8-15
      val rs1t = ui(9, 7) + 8.U
      val rs2t = ui(4, 2) + 8.U

      val rs1e = ui(11, 7)
      val rs2e = ui(6, 2)

      // Helper for assigning unsigned immediates
      val uimm = Wire(UInt(32.W))
      result.imm := uimm.asSInt()
      uimm := DontCare

      // RD position will differ by instr

      def fail() = {
        result.info := DecodeInfo.invalid
        result.base := InstrType.RESERVED
        result.imm := 0.S

        // illegal instruction
        result.funct7 := 0.U
        result.rs2 := 0.U
        result.rs1 := 0.U
        result.rd := 0.U
        result.funct3 := 0.U
        result.op := 0.U
      }

      // Table 12.3 RVC opcode map
      switch(ui(1, 0) ## ui(15, 13)) {
        is("00000".asBits(5.W)) { // ADDI4SPN
          result.op := Op("OP-IMM").ident
          result.rd := rs2t
          result.rs1 := 2.U // sp
          result.rs2 := DontCare
          uimm := ui(10, 7) ## ui(12, 11) ## ui(5) ## ui(6) ## 0.U(2.W)
          result.funct3 := OP_FUNC("ADD/SUB") // ADDI
          result.info := DecodeInfo.assign(Instructions.ADDI)
        }
        is("00001".asBits(5.W)) { // FLD
          fail()
        }
        is("00010".asBits(5.W)) { // LW
          result.op := Op("LOAD").ident
          result.rs1 := rs1t
          result.rs2 := DontCare
          result.rd := rs2t
          uimm := ui(5) ## ui(12, 10) ## ui(6) ## 0.U(2.W)
          result.funct3 := MEM_WIDTH_FUNC("W")
          result.info := DecodeInfo.assign(Instructions.LW)
        }
        is("00011".asBits(5.W)) { // LD
          result.op := Op("LOAD").ident
          result.rs1 := rs1t
          result.rs2 := DontCare
          result.rd := rs2t
          uimm := ui(6, 5) ## ui(12, 10) ## 0.U(3.W)
          result.funct3 := MEM_WIDTH_FUNC("D")
          result.info := DecodeInfo.assign(Instructions.LD)
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
          uimm := ui(5) ## ui(12, 10) ## ui(6) ## 0.U(2.W)
          result.funct3 := MEM_WIDTH_FUNC("W")
          result.info := DecodeInfo.assign(Instructions.SW)
        }
        is("00111".asBits(5.W)) { // SD
          result.op := Op("STORE").ident
          result.rs1 := rs1t
          result.rs2 := rs2t
          result.rd := DontCare
          uimm := ui(6, 5) ## ui(12, 10) ## 0.U(3.W)
          result.funct3 := MEM_WIDTH_FUNC("D")
          result.info := DecodeInfo.assign(Instructions.SD)
        }

        is("01000".asBits(5.W)) { // ADDI
          result.op := Op("OP-IMM").ident
          result.rd := rs1e
          result.rs1 := rs1e
          result.rs2 := DontCare
          result.imm := (ui(12) ## ui(6, 2)).asSInt
          result.funct3 := OP_FUNC("ADD/SUB") // Can only be ADDI
          result.info := DecodeInfo.assign(Instructions.ADDI)
        }
        is("01001".asBits(5.W)) { // ADDIW
          result.op := Op("OP-IMM-32").ident
          result.rd := rs1e
          result.rs1 := rs1e
          result.rs2 := DontCare
          result.imm := (ui(12) ## ui(6, 2)).asSInt
          result.funct3 := OP_FUNC("ADD/SUB") // Can only be ADDI
          result.info := DecodeInfo.assign(Instructions.ADDIW)
        }
        is("01010".asBits(5.W)) { // LI, encode as ori rd, x0, imm
          result.op := Op("OP-IMM").ident
          result.rd := rs1e
          result.rs1 := 0.U
          result.rs2 := DontCare
          result.imm := (ui(12) ## ui(6, 2)).asSInt
          result.funct3 := OP_FUNC("OR")
          result.info := DecodeInfo.assign(Instructions.ORI)
        }
        is("01011".asBits(5.W)) { // LUI/ADDI16SP
          when(rs1e === 2.U) { // ADDI16SP
            result.op := Op("OP-IMM").ident
            result.rd := 2.U // sp
            result.rs1 := 2.U // sp
            result.rs2 := DontCare
            result.imm := (ui(12) ## ui(4, 3) ## ui(5) ## ui(2) ## ui(
              6
            )).asSInt << 4
            result.funct3 := OP_FUNC("ADD/SUB") // ADDI
            result.info := DecodeInfo.assign(Instructions.ADDI)
          }.otherwise { // LUI
            result.op := Op("LUI").ident
            result.rd := rs1e
            result.rs1 := DontCare
            result.rs2 := DontCare
            result.imm := (ui(12) ## ui(6, 2)).asSInt << 12
            result.funct3 := DontCare
            result.info := DecodeInfo.assign(Instructions.LUI)
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
            result.funct7 := ui(11, 10) ## 0.U(5.W)
            when(ui(10) === 0.U) {
              result.info := DecodeInfo.assign(Instructions.SRLI)
            } otherwise {
              result.info := DecodeInfo.assign(Instructions.SRAI)
            }
          }.elsewhen(ui(10) === 0.U) { // ANDI
            result.op := Op("OP-IMM").ident
            result.rs1 := rs1t
            result.rs2 := DontCare
            result.rd := rs1t
            result.imm := (ui(12) ## ui(6, 2)).asSInt
            result.funct3 := OP_FUNC("AND")
            result.info := DecodeInfo.assign(Instructions.ANDI)
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
                result.info := DecodeInfo.assign(Instructions.SUB)
              }

              is("001".asBits(3.W)) { // XOR
                result.funct3 := OP_FUNC("XOR")
                result.info := DecodeInfo.assign(Instructions.XOR)
              }

              is("010".asBits(3.W)) { // OR
                result.funct3 := OP_FUNC("OR")
                result.info := DecodeInfo.assign(Instructions.OR)
              }

              is("011".asBits(3.W)) { // AND
                result.funct3 := OP_FUNC("AND")
                result.info := DecodeInfo.assign(Instructions.AND)
              }

              is("100".asBits(3.W)) { // SUBW
                result.op := Op("OP-32").ident
                result.funct3 := OP_FUNC("ADD/SUB")
                result.funct7 := "0100000".asBits(7.W)
                result.info := DecodeInfo.assign(Instructions.SUBW)
              }

              is("101".asBits(3.W)) { // ADDW
                result.op := Op("OP-32").ident
                result.funct3 := OP_FUNC("ADD/SUB")
                result.funct7 := "0000000".asBits(7.W)
                result.info := DecodeInfo.assign(Instructions.ADDW)
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
            ui(12) ## ui(8) ## ui(10, 9) ## ui(6) ## ui(7) ## ui(2) ## ui(
              11
            ) ## ui(5, 3) ## 0.U(1.W)
          ).asSInt
          result.funct3 := DontCare // TODO: trigger error on treadle when reading DontCare
          result.info := DecodeInfo.assign(Instructions.JAL)
        }
        is("01110".asBits(5.W)) { // BEQZ
          result.op := Op("BRANCH").ident
          result.rs1 := rs1t
          result.rs2 := 0.U // Compare with zero
          result.rd := DontCare
          result.imm := (ui(12) ## ui(6, 5) ## ui(2) ## ui(11, 10) ## ui(
            4,
            3
          ) ## 0.U(1.W)).asSInt
          result.funct3 := BRANCH_FUNC("BEQ")
          result.info := DecodeInfo.assign(Instructions.BEQ)
        }
        is("01111".asBits(5.W)) { // BNEZ
          result.op := Op("BRANCH").ident
          result.rs1 := rs1t
          result.rs2 := 0.U // Compare with zero
          result.rd := DontCare
          result.imm := (ui(12) ## ui(6, 5) ## ui(2) ## ui(11, 10) ## ui(
            4,
            3
          ) ## 0.U(1.W)).asSInt
          result.funct3 := BRANCH_FUNC("BNE")
          result.info := DecodeInfo.assign(Instructions.BNE)
        }

        is("10000".asBits(5.W)) { // SLLI
          result.imm := (0.U(1.W) ## ui(12) ## ui(6, 2)).asSInt
          result.rs1 := rs1e
          result.rd := rs1e
          result.funct3 := OP_FUNC("SLL")
          result.op := Op("OP-IMM").ident
          result.info := DecodeInfo.assign(Instructions.SLLI)
        }
        is("10001".asBits(5.W)) { // FLDSP
          fail()
        }
        is("10010".asBits(5.W)) { // LWSP
          result.op := Op("LOAD").ident
          result.rs1 := 2.U // x2 = sp
          result.rs2 := DontCare
          result.rd := rs1e
          uimm := ui(3, 2) ## ui(12) ## ui(6, 4) ## 0.U(2.W)
          result.funct3 := MEM_WIDTH_FUNC("W")
          result.info := DecodeInfo.assign(Instructions.LW)
        }
        is("10011".asBits(5.W)) { // LDSP
          result.op := Op("LOAD").ident
          result.rs1 := 2.U // x2 = sp
          result.rs2 := DontCare
          result.rd := rs1e
          uimm := ui(4, 2) ## ui(12) ## ui(6, 5) ## 0.U(3.W)
          result.funct3 := MEM_WIDTH_FUNC("D")
          result.info := DecodeInfo.assign(Instructions.LD)
        }
        is("10100".asBits(5.W)) { // J[AL]R/MV/ADD
          when(ui(12) === 0.U) {
            when(rs2e === 0.U) { // JR
              result.imm := 0.S

              result.rs2 := 0.U
              result.rs1 := rs1e
              result.rd := 0.U // Ignore result
              result.op := Op("JALR").ident
              result.info := DecodeInfo.assign(Instructions.JALR)
            }.otherwise { // MV, encode as or rd, x0, rs2, same as add rd, x0, rs2
              result.op := Op("OP").ident
              result.rd := rs1e
              result.rs1 := 0.U
              result.rs2 := rs2e
              result.imm := DontCare
              result.funct3 := OP_FUNC("OR")
              result.info := DecodeInfo.assign(Instructions.OR)
            }
          }.otherwise {
            when(rs2e === 0.U && rs1e === 0.U) { // EBREAK
              result.op := Op("SYSTEM").ident
              result.rs1 := 0.U
              result.rs2 := PRIV_RS2("EBREAK")
              result.rd := 0.U
              result.imm := 0.S
              result.funct3 := 0.U
              result.funct7 := 0.U
              result.info := DecodeInfo.assign(Instructions.EBREAK)
            }.elsewhen(rs2e === 0.U) { // JALR
              result.op := Op("JALR").ident
              result.rs1 := rs1e
              result.rs2 := DontCare
              result.rd := 1.U // x1 = ra
              result.imm := 0.S
              result.funct3 := DontCare
              result.info := DecodeInfo.assign(Instructions.JALR)
            }.otherwise { // ADD
              result.op := Op("OP").ident
              result.rs1 := rs1e
              result.rs2 := rs2e
              result.rd := rs1e
              result.imm := DontCare
              result.funct3 := OP_FUNC("ADD/SUB")
              result.funct7 := "0000000".asBits(7.W)
              result.info := DecodeInfo.assign(Instructions.ADD)
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
          uimm := ui(8, 7) ## ui(12, 9) ## 0.U(2.W)
          result.funct3 := MEM_WIDTH_FUNC("W")
          result.info := DecodeInfo.assign(Instructions.SW)
        }
        is("10111".asBits(5.W)) { // SDSP
          result.op := Op("STORE").ident
          result.rs1 := 2.U // x2 = sp
          result.rs2 := rs2e
          result.rd := DontCare
          uimm := ui(9, 7) ## ui(12, 10) ## 0.U(3.W)
          result.funct3 := MEM_WIDTH_FUNC("D")
          result.info := DecodeInfo.assign(Instructions.SD)
        }
      }

      result
    }

    def asInstr32(): Instr = {
      val ui = self.asUInt
      val result = Wire(new Instr)
      result.raw := ui
      result.op := ui >> 2
      result.info := DecodeInfo.decode(ui)

      var ctx: Option[WhenContext] = None

      for ((op, OpSpec(pat, typ)) <- Op) {
        ctx = Some(ctx match {
          case Some(c) => c.elsewhen(result.op === pat) { result.base := typ }
          case None    => when(result.op === pat) { result.base := typ }
        })
      }

      ctx.get.otherwise { result.base := InstrType.RESERVED }

      // Really parse the instr
      result.funct3 := ui(14, 12)
      result.funct7 := ui(31, 25)
      result.rd := ui(11, 7)
      result.rs1 := ui(19, 15)
      result.rs2 := ui(24, 20)

      // Parse immediate
      result.imm := 0.S // For R-type
      switch(result.base) {
        is(InstrType.I) {
          result.imm := ui(31, 20).asSInt
        }
        is(InstrType.S) {
          result.imm := (ui(31, 25) ## ui(11, 7)).asSInt
        }
        is(InstrType.B) {
          result.imm := (ui(31) ## ui(7) ## ui(30, 25) ## ui(11, 8) ## 0.U(
            1.W
          )).asSInt
        }
        is(InstrType.U) {
          result.imm := ui(31, 12).asSInt << 12
        }
        is(InstrType.J) {
          result.imm := (ui(31) ## ui(19, 12) ## ui(20) ## ui(30, 21) ## 0.U(
            1.W
          )).asSInt
        }
      }

      result
    }
  }
}

/** Register index with its type
  */
class RegIndex extends Bundle {
  val ty = RegType()
  val index = UInt(5.W)
}

object RegIndex {
  def create(ty: RegType.Type, index: UInt) = {
    val res = Wire(new RegIndex())
    res.ty := ty
    res.index := index
    res
  }
}

/** Raw RISC-V Instruction
  */
class Instr extends Bundle {
  // Raw Instruction
  // for Simulation
  val raw = UInt(32.W)

  val info = new DecodeInfo

  // Opcode
  val op = UInt(5.W)
  val base = InstrType()

  // Immediate
  val imm = SInt(32.W)

  // Registers
  val rs1 = UInt(5.W)
  val rs2 = UInt(5.W)
  val rd = UInt(5.W)

  // Funct
  val funct7 = UInt(7.W)
  val funct3 = UInt(3.W)

  // for R4-type
  def funct5() = funct7 >> 2
  // floating point format
  def fmt() = funct7(1, 0)

  override def toPrintable: Printable = {
    // Reverse check op
    p"Instr: \n" +
      p"  Base: ${Decimal(base.asUInt())}\n" +
      p"  Op:   0b${Binary(op)}\n" +
      p"  Imm:  0x${Hexadecimal(imm)}\n" +
      p"  RS1:  x${Decimal(rs1)}\n" +
      p"  RS2:  x${Decimal(rs2)}\n" +
      p"  RD:   x${Decimal(rd)}\n" +
      p"  F7:   0b${Binary(funct7)}\n" +
      p"  F3:   0b${Binary(funct3)}"
  }

  def getRdIndex() = Mux(info.writeRd, rd, 0.U)

  def getRdType() = WireInit(this.info.rdType)

  def getRd() = RegIndex.create(getRdType(), getRdIndex())

  def getRs1Index() = Mux(info.readRs1, rs1, 0.U)

  def getRs1Type() = WireInit(this.info.rs1Type)

  def getRs1() = RegIndex.create(getRs1Type(), getRs1Index())

  def getRs2Index() = Mux(info.readRs2, rs2, 0.U)

  def getRs2Type() = WireInit(this.info.rs2Type)

  def getRs2() = RegIndex.create(getRs2Type(), getRs2Index())

}
