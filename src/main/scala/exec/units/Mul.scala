package exec.units

import chisel3._
import chisel3.util._
import instr.Decoder
import exec._
import _root_.core.CoreDef
import Chisel.experimental.chiselName

class MulExt(implicit val coredef: CoreDef) extends Bundle {
  val x1 = UInt((coredef.XLEN).W)
  val x2 = UInt((coredef.XLEN).W)

  val mid1 = UInt((coredef.XLEN).W)
  val mid2 = UInt((coredef.XLEN).W)

  val neg = Bool()
}

@chiselName
class Mul(override implicit val coredef: CoreDef) extends ExecUnit(2, new MulExt) {
  assert(coredef.XLEN == 64)

  override def map(stage: Int, pipe: PipeInstr, _ext: Option[MulExt]): (MulExt, Bool) = {
    val isDWord = (
      pipe.instr.instr.op === Decoder.Op("OP-IMM").ident
      || pipe.instr.instr.op === Decoder.Op("OP").ident
    )

    if(stage == 0) {
      // Pipelining requests
      val ext = Wire(new MulExt)

      val (op1, op2) = (Wire(SInt(coredef.XLEN.W)), Wire(SInt(coredef.XLEN.W)))

      when(isDWord) {
        op1 := pipe.rs1val.asSInt
        op2 := pipe.rs2val.asSInt
      }.otherwise {
        op1 := pipe.rs1val(31, 0).asSInt
        op2 := pipe.rs2val(31, 0).asSInt
      }

      when(isDWord) {
        ext.neg := DontCare
        ext.x1 := DontCare
        ext.x2 := DontCare

        switch(pipe.instr.instr.funct3) {
          is(Decoder.MULDIV_FUNC("MUL")) {
            ext.neg := false.B
            ext.x1 := op1.asUInt
            ext.x2 := op2.asUInt
          }

          is(Decoder.MULDIV_FUNC("MULH")) {
            ext.neg := op1(63) ^ op2(63)
            ext.x1 := op1.abs().asUInt
            ext.x2 := op2.abs().asUInt
          }

          is(Decoder.MULDIV_FUNC("MULHU")) {
            ext.neg := false.B
            ext.x1 := op1.asUInt
            ext.x2 := op2.asUInt
          }

          is(Decoder.MULDIV_FUNC("MULHSU")) {
            ext.neg := op1(63)
            ext.x1 := op1.abs().asUInt
            ext.x2 := op2.asUInt
          }
        }
      }.otherwise {
        ext.neg := DontCare
        ext.x1 := op1.asUInt
        ext.x2 := op2.asUInt
      }

      ext.mid1 := DontCare
      ext.mid2 := DontCare

      return (ext, false.B)
    } else if(stage == 1) {
      // printf(p"[MUL  0]: COMP ${Hexadecimal(pipe.rs1val)} * ${Hexadecimal(pipe.rs2val)}\n")
      val prev = _ext.get
      val ext = Wire(new MulExt)
      ext.neg := prev.neg

      ext.x1 := prev.x1(31, 0) * prev.x2(31, 0)
      ext.mid1 := prev.x1(63, 32) * prev.x2(31, 0)
      ext.mid2 := prev.x1(31, 0) * prev.x2(63, 32)
      ext.x2 := prev.x1(63, 32) * prev.x2(63, 32)

      (ext, false.B)
    } else if(stage == 2) {
      val prev = _ext.get
      val ext = Wire(new MulExt)
      ext := DontCare

      when(!isDWord) {
        // Can only be MULW
        val extended = Wire(SInt(coredef.XLEN.W))
        extended := prev.x1(31, 0).asSInt
        ext.x1 := extended.asUInt()
      }.otherwise{
        val added = Wire(UInt((coredef.XLEN*2).W))
        added := prev.x1 +& ((prev.mid1 +& prev.mid2) << 32) +& (prev.x2 << 64)
        when(pipe.instr.instr.funct3 === Decoder.MULDIV_FUNC("MUL")) {
          ext.x1 := added(63, 0)
        }.otherwise {
          val signed = added.asSInt

          when(prev.neg) {
            ext.x1 := (-signed).asUInt()(127, 64)
          }.otherwise {
            ext.x1 := signed(127, 64)
          }
        }
      }

      (ext, false.B)
    } else {
      throw new Error(s"Unexpected stage $stage in Mul module")
    }
  }

  override def finalize(pipe: PipeInstr, ext: MulExt): RetireInfo = {
    val info = WireDefault(RetireInfo.vacant)

    info.wb := ext.x1

    info
  }

  init()
}
