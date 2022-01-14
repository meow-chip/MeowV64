package meowv64.instr

import chisel3._
import chisel3.experimental._
import chisel3.internal.firrtl.Width
import chisel3.util._
import meowv64.instr.Decoder.InstrType
import meowv64.reg.RegType

class DecodeInfo extends Bundle {
  // legal instruction
  val legal = Bool()

  // register related
  // writes to rd
  val write = Bool()
  // reads rs1
  val readRs1 = Bool()
  // reads rs2
  val readRs2 = Bool()
  // rd is integer
  val rdInt = Bool()
  // rd is float
  val rdFloat = Bool()
  // rs1 is integer
  val rs1Int = Bool()
  // rs1 is float
  val rs1Float = Bool()
  // rs2 is integer
  val rs2Int = Bool()
  // rs2 is float
  val rs2Float = Bool()

  // execution unit related
  val gotoALU = Bool()
  val gotoBranch = Bool()
  val gotoBypass = Bool()
  val gotoCSR = Bool()
  val gotoDiv = Bool()
  val gotoMul = Bool()
  val gotoFMA = Bool()
  val gotoLSU = Bool()
}

object Instructions {
  // RV32I Base Instruction Set
  val LUI = BitPat("?????????????????????????0110111")
  val AUIPC = BitPat("?????????????????????????0010111")

  val JAL = BitPat("?????????????????????????1101111")
  val JALR = BitPat("?????????????????000?????1100111")

  val BEQ = BitPat("?????????????????000?????1100011")
  val BNE = BitPat("?????????????????001?????1100011")
  val BLT = BitPat("?????????????????100?????1100011")
  val BGE = BitPat("?????????????????111?????1100011")
  val BLTU = BitPat("?????????????????110?????1100011")
  val BGEU = BitPat("?????????????????111?????1100011")

  val LB = BitPat("?????????????????000?????0000011")
  val LH = BitPat("?????????????????001?????0000011")
  val LW = BitPat("?????????????????010?????0000011")
  val LBU = BitPat("?????????????????100?????0000011")
  val LHU = BitPat("?????????????????101?????0000011")

  val SB = BitPat("?????????????????000?????0100011")
  val SH = BitPat("?????????????????001?????0100011")
  val SW = BitPat("?????????????????010?????0100011")

  val ADDI = BitPat("?????????????????000?????0010011")
  val SLTI = BitPat("?????????????????010?????0010011")
  val SLTIU = BitPat("?????????????????011?????0010011")
  val XORI = BitPat("?????????????????100?????0010011")
  val ORI = BitPat("?????????????????110?????0010011")
  val ANDI = BitPat("?????????????????111?????0010011")

  // one more shamt bit in rv64
  val SLLI = BitPat("000000???????????001?????0010011")
  val SRLI = BitPat("000000???????????101?????0010011")
  val SRAI = BitPat("010000???????????101?????0010011")

  val ADD = BitPat("0000000??????????000?????0110011")
  val SUB = BitPat("0100000??????????000?????0110011")
  val SLL = BitPat("0000000??????????001?????0110011")
  val SLT = BitPat("0000000??????????010?????0110011")
  val SLTU = BitPat("0000000??????????011?????0110011")
  val XOR = BitPat("0000000??????????100?????0110011")
  val SRL = BitPat("0000000??????????101?????0110011")
  val SRA = BitPat("0100000??????????101?????0110011")
  val OR = BitPat("0000000??????????110?????0110011")
  val AND = BitPat("0000000??????????111?????0110011")

  val FENCE = BitPat("0000????????00000000000000001111")
  val FENCE_I = BitPat("00000000000000000001000000001111")

  val ECALL = BitPat("00000000000000000000000001110011")
  val EBREAK = BitPat("00000000000100000000000001110011")

  val CSRRW = BitPat("?????????????????001?????1110011")
  val CSRRS = BitPat("?????????????????010?????1110011")
  val CSRRC = BitPat("?????????????????011?????1110011")
  val CSRRWI = BitPat("?????????????????101?????1110011")
  val CSRRSI = BitPat("?????????????????110?????1110011")
  val CSRRCI = BitPat("?????????????????111?????1110011")

  // RV64I Base Instruction Set (in addition to RV32I)
  val LWU = BitPat("?????????????????110?????0000011")
  val LD = BitPat("?????????????????011?????0000011")
  val SD = BitPat("?????????????????011?????0100011")

  val ADDIW = BitPat("?????????????????000?????0011011")
  val SLLIW = BitPat("0000000??????????001?????0011011")
  val SRLIW = BitPat("0000000??????????101?????0011011")
  val SRAIW = BitPat("0100000??????????101?????0011011")

  val ADDW = BitPat("0000000??????????000?????0111011")
  val SUBW = BitPat("0100000??????????000?????0111011")
  val SLLW = BitPat("0000000??????????001?????0111011")
  val SRLW = BitPat("0000000??????????101?????0111011")
  val SRAW = BitPat("0100000??????????101?????0111011")

  // RV32M Standard Extension
  val MUL = BitPat("0000001??????????000?????0110011")
  val MULH = BitPat("0000001??????????001?????0110011")
  val MULHSU = BitPat("0000001??????????010?????0110011")
  val MULHU = BitPat("0000001??????????011?????0110011")
  val DIV = BitPat("0000001??????????100?????0110011")
  val DIVU = BitPat("0000001??????????101?????0110011")
  val REM = BitPat("0000001??????????110?????0110011")
  val REMU = BitPat("0000001??????????111?????0110011")

  // RV64M Standard Extension (in addition to RV32M)
  val MULW = BitPat("0000001??????????000?????0111011")
  val DIVW = BitPat("0000001??????????100?????0111011")
  val DIVUW = BitPat("0000001??????????101?????0111011")
  val REMW = BitPat("0000001??????????110?????0111011")
  val REMUW = BitPat("0000001??????????111?????0111011")

  // RV32A Standard Extension
  val LR_W = BitPat("00010??00000?????010?????0101111")
  val SC_W = BitPat("00011????????????010?????0101111")
  val AMOSWAP_W = BitPat("00001????????????010?????0101111")
  val AMOADD_W = BitPat("00000????????????010?????0101111")
  val AMOXOR_W = BitPat("00100????????????010?????0101111")
  val AMOAND_W = BitPat("01100????????????010?????0101111")
  val AMOOR_W = BitPat("01000????????????010?????0101111")
  val AMOMIN_W = BitPat("10000????????????010?????0101111")
  val AMOMAX_W = BitPat("10100????????????010?????0101111")
  val AMOMINU_W = BitPat("11000????????????010?????0101111")
  val AMOMAXU_W = BitPat("11100????????????010?????0101111")

  // RV64A Standard Extension (in addition to RV32A)
  val LR_D = BitPat("00010??00000?????011?????0101111")
  val SC_D = BitPat("00011????????????011?????0101111")
  val AMOSWAP_D = BitPat("00001????????????011?????0101111")
  val AMOADD_D = BitPat("00000????????????011?????0101111")
  val AMOXOR_D = BitPat("00100????????????011?????0101111")
  val AMOAND_D = BitPat("01100????????????011?????0101111")
  val AMOOR_D = BitPat("01000????????????011?????0101111")
  val AMOMIN_D = BitPat("10000????????????011?????0101111")
  val AMOMAX_D = BitPat("10100????????????011?????0101111")
  val AMOMINU_D = BitPat("11000????????????011?????0101111")
  val AMOMAXU_D = BitPat("11100????????????011?????0101111")
  
  // RV32F Standard Extension
  val FLW = BitPat("?????????????????010?????0000111")
  val FSW = BitPat("?????????????????010?????0100111")

  val FMADD_S = BitPat("?????00??????????????????1000011")
  val FMSUB_S = BitPat("?????00??????????????????1000111")
  val FNMSUB_S = BitPat("?????00??????????????????1001011")
  val FNMADD_S = BitPat("?????00??????????????????1001111")

  val FADD_S = BitPat("0000000??????????????????1010011")
  val FSUB_S = BitPat("0000100??????????????????1010011")
  val FMUL_S = BitPat("0001000??????????????????1010011")
  val FDIV_S = BitPat("0001100??????????????????1010011")
  val FSQRT_S = BitPat("010110000000?????????????1010011")
  val FSGNJ_S = BitPat("0010000??????????000?????1010011")
  val FSGNJN_S = BitPat("0010000??????????001?????1010011")
  val FSGNJX_S = BitPat("0010000??????????010?????1010011")
  val FMIN_S = BitPat("0010100??????????000?????1010011")
  val FMAX_S = BitPat("0010100??????????001?????1010011")
  val FCVT_W_S = BitPat("110000000000?????????????1010011")
  val FCVT_WU_S = BitPat("110000000001?????????????1010011")
  val FMV_X_W = BitPat("111000000000?????????????1010011")
  val FEQ_S = BitPat("1010000??????????010?????1010011")
  val FLT_S = BitPat("1010000??????????001?????1010011")
  val FLE_S = BitPat("1010000??????????000?????1010011")
  val FCLASS_S = BitPat("111000000000?????001?????1010011")
  val FCVT_S_W = BitPat("110100000000?????????????1010011")
  val FCVT_S_WU = BitPat("110100000001?????????????1010011")
  val FMV_W_X = BitPat("111100000000?????000?????1010011")

  // RV64F Standard Extension (in addition to RV32F)
  val FCVT_L_S = BitPat("110000000010?????????????1010011")
  val FCVT_LU_S = BitPat("110000000011?????????????1010011")
  val FCVT_S_L = BitPat("110100000010?????????????1010011")
  val FCVT_S_LU = BitPat("110100000011?????????????1010011")

  // RV32D Standard Extension
  val FLD = BitPat("?????????????????011?????0000111")
  val FSD = BitPat("?????????????????011?????0100111")

  val FMADD_D = BitPat("?????01??????????????????1000011")
  val FMSUB_D = BitPat("?????01??????????????????1000111")
  val FNMSUB_D = BitPat("?????01??????????????????1001011")
  val FNMADD_D = BitPat("?????01??????????????????1001111")

  val FADD_D = BitPat("0000001??????????????????1010011")
  val FSUB_D = BitPat("0000101??????????????????1010011")
  val FMUL_D = BitPat("0001001??????????????????1010011")
  val FDIV_D = BitPat("0001101??????????????????1010011")
  val FSQRT_D = BitPat("010110100000?????????????1010011")
  val FSGNJ_D = BitPat("0010001??????????000?????1010011")
  val FSGNJN_D = BitPat("0010001??????????001?????1010011")
  val FSGNJX_D = BitPat("0010001??????????010?????1010011")
  val FMIN_D = BitPat("0010101??????????000?????1010011")
  val FMAX_D = BitPat("0010101??????????001?????1010011")
  val FCVT_S_D = BitPat("010000000001?????????????1010011")
  val FCVT_D_S = BitPat("010000100000?????????????1010011")
  val FEQ_D = BitPat("1010001??????????010?????1010011")
  val FLT_D = BitPat("1010001??????????001?????1010011")
  val FLE_D = BitPat("1010001??????????000?????1010011")
  val FCLASS_D = BitPat("111000100000?????001?????1010011")
  val FCVT_W_D = BitPat("110000100000?????????????1010011")
  val FCVT_WU_D = BitPat("110000100001?????????????1010011")
  val FCVT_D_W = BitPat("110100100000?????????????1010011")
  val FCVT_D_WU = BitPat("110100100001?????????????1010011")

  // RV64D Standard Extension (in addition to RV32D)
  val FCVT_L_D = BitPat("110000100010?????????????1010011")
  val FCVT_LU_D = BitPat("110000100011?????????????1010011")
  val FMV_X_D = BitPat("111000100000?????000?????1010011")
  val FCVT_D_L = BitPat("110100100010?????????????1010011")
  val FCVT_D_LU = BitPat("110100100011?????????????1010011")
  val FMV_D_X = BitPat("111100100000?????000?????1010011")
}

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
    "FCMP" -> "10100",
    "FMV.X.D" -> "11100",
    "FCLASS" -> "11100",
    "FMV.D.X" -> "11110"
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
      }.elsewhen(ui(1, 0) =/= "11".asBits(2.W)) {
        result := self.asInstr16()
        isInstr16 := true.B
      }.otherwise {
        result := self.asInstr32()
      }

      (result, isInstr16)
    }

    def tryAsInstr16(): (Instr, Bool) = {
      assert(
        self.getWidth == 16,
        s"Unexpected decoder input width: ${self.getWidth}"
      )
      val result = Wire(new Instr)
      val success = Wire(Bool())
      val ui = self.asUInt

      when(!ui.orR()) {
        // Defined invalid instr
        result := DontCare
        success := true.B
        result.base := InstrType.RESERVED
      }.elsewhen(ui(1, 0) =/= "11".asBits(2.W)) {
        result := self.asInstr16
        success := true.B
      }.otherwise {
        result := DontCare
        success := false.B
      }

      (result, success)
    }

    def asInstr16(): Instr = {
      val result = Wire(new Instr)
      result := DontCare
      result.base := InstrType.C

      val ui = self.asUInt
      result.raw := ui

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
        result.base := InstrType.RESERVED
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
          uimm := ui(10, 7) ## ui(12, 11) ## ui(5) ## ui(6) ## 0.U(2.W)
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
          uimm := ui(5) ## ui(12, 10) ## ui(6) ## 0.U(2.W)
          result.funct3 := MEM_WIDTH_FUNC("W")
        }
        is("00011".asBits(5.W)) { // LD
          result.op := Op("LOAD").ident
          result.rs1 := rs1t
          result.rs2 := DontCare
          result.rd := rs2t
          uimm := ui(6, 5) ## ui(12, 10) ## 0.U(3.W)
          result.funct3 := MEM_WIDTH_FUNC("D")
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
        }
        is("00111".asBits(5.W)) { // SD
          result.op := Op("STORE").ident
          result.rs1 := rs1t
          result.rs2 := rs2t
          result.rd := DontCare
          uimm := ui(6, 5) ## ui(12, 10) ## 0.U(3.W)
          result.funct3 := MEM_WIDTH_FUNC("D")
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
            result.imm := (ui(12) ## ui(4, 3) ## ui(5) ## ui(2) ## ui(
              6
            )).asSInt << 4
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
            result.funct7 := ui(11, 10) ## 0.U(5.W)
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
            ui(12) ## ui(8) ## ui(10, 9) ## ui(6) ## ui(7) ## ui(2) ## ui(
              11
            ) ## ui(5, 3) ## 0.U(1.W)
          ).asSInt
          result.funct3 := DontCare // TODO: trigger error on treadle when reading DontCare
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
          uimm := ui(3, 2) ## ui(12) ## ui(6, 4) ## 0.U(2.W)
          result.funct3 := MEM_WIDTH_FUNC("W")
        }
        is("10011".asBits(5.W)) { // LDSP
          result.op := Op("LOAD").ident
          result.rs1 := 2.U // x2 = sp
          result.rs2 := DontCare
          result.rd := rs1e
          uimm := ui(4, 2) ## ui(12) ## ui(6, 5) ## 0.U(3.W)
          result.funct3 := MEM_WIDTH_FUNC("D")
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
          }.otherwise {
            when(rs2e === 0.U && rs1e === 0.U) { // EBREAK
              result.op := Op("SYSTEM").ident
              result.rs1 := 0.U
              result.rs2 := PRIV_RS2("EBREAK")
              result.rd := 0.U
              result.imm := 0.S
              result.funct3 := 0.U
              result.funct7 := 0.U
            }.elsewhen(rs2e === 0.U) { // JALR
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
              result.funct7 := "0000000".asBits(7.W)
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
        }
        is("10111".asBits(5.W)) { // SDSP
          result.op := Op("STORE").ident
          result.rs1 := 2.U // x2 = sp
          result.rs2 := rs2e
          result.rd := DontCare
          uimm := ui(9, 7) ## ui(12, 10) ## 0.U(3.W)
          result.funct3 := MEM_WIDTH_FUNC("D")
        }
      }

      result
    }

    def asInstr32(): Instr = {
      val ui = self.asUInt
      val result = Wire(new Instr)
      result.raw := ui
      result.op := ui >> 2

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

  // Opcode
  val op = UInt(5.W)
  val base = InstrType()

  // Immediates
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

  def getRdIndex() = {
    val ret = WireDefault(rd)
    // B-types and S-types don't have rd
    switch(this.op) {
      is(Decoder.Op("BRANCH").ident, Decoder.Op("STORE").ident) {
        ret := 0.U
      }
    }

    ret
  }

  def getRdType() = {
    val ret = WireDefault(RegType.integer)

    switch(this.op) {
      is(
        Decoder.Op("OP-FP").ident,
        Decoder.Op("LOAD-FP").ident // FLD
      ) {
        when(
          this.funct5() === Decoder.FP_FUNC("FMV.X.D") ||
            this.funct5() === Decoder.FP_FUNC("FCMP")
        ) {
          ret := RegType.integer
        }.otherwise {
          ret := RegType.float
        }
      }
    }

    ret
  }

  def getRd() = RegIndex.create(getRdType(), getRdIndex())

  def getRs1Index() = {
    val ret = WireDefault(rs1)
    // The only instructions that don't have RS1 is AUIPC/LUI and JAL (U and J type)

    // TODO: investigate if SYSTEM instrs can have a rs1 field containing non-zero values?

    switch(this.op) {
      is(
        Decoder.Op("LUI").ident,
        Decoder.Op("AUIPC").ident,
        Decoder.Op("JAL").ident
      ) {
        ret := 0.U
      }
    }

    ret
  }

  def getRs1Type() = {
    val ret = WireDefault(RegType.integer)

    switch(this.op) {
      is(
        Decoder.Op("OP-FP").ident
      ) {
        when(this.funct5() === Decoder.FP_FUNC("FMV.D.X")) {
          ret := RegType.integer
        }.otherwise {
          ret := RegType.float
        }
      }
    }

    ret
  }

  def getRs1() = RegIndex.create(getRs1Type(), getRs1Index())

  def getRs2Index() = {
    val ret = WireDefault(rs2)
    switch(this.op) {
      // U-type and J-type
      is(
        Decoder.Op("LUI").ident,
        Decoder.Op("AUIPC").ident,
        Decoder.Op("JAL").ident
      ) {
        ret := 0.U
      }

      // I-type
      is(
        Decoder.Op("LOAD").ident,
        Decoder.Op("MISC-MEM").ident,
        Decoder.Op("OP-IMM").ident,
        Decoder.Op("OP-IMM-32").ident,
        Decoder.Op("JALR").ident,
        Decoder.Op("SYSTEM").ident
      ) {
        ret := 0.U
      }
    }
    ret
  }

  def getRs2Type() = {
    val ret = WireDefault(RegType.integer)

    switch(this.op) {
      is(
        Decoder.Op("OP-FP").ident,
        Decoder.Op("STORE-FP").ident // FST
      ) {
        ret := RegType.float
      }
    }

    ret
  }

  def getRs2() = RegIndex.create(getRs2Type(), getRs2Index())

}
