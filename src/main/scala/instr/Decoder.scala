package instr

import chisel3._
import chisel3.util._

/**
 * Instruction Decoder
 * We only supports 32-bit length instructions
 */

object Decoder {
  object InstrType extends Enumeration {
    type InstrType = Value
    val RESERVED, R, I, S, B, U, J = Value

    def toInt(x: Value) = x match {
      case RESERVED => 0.U(3.W)
      case R => 1.U(3.W)
      case I => 2.U(3.W)
      case S => 3.U(3.W)
      case B => 4.U(3.W)
      case U => 5.U(3.W)
      case J => 6.U(3.W)
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
    "BLT" -> "010",
    "BGE" -> "011",
    "BLTU" -> "100",
    "BGEU" -> "110"
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

  implicit class ConvertToInstr(self: Data) {
    def asInstr(): Instr = {
      val result = Wire(new Instr)
      val ui = self.asUInt
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
