package meowv64.exec.units

import chisel3._
import chisel3.util._
import meowv64.instr.Decoder
import meowv64.exec._
import meowv64.core.CoreDef
import Chisel.experimental.chiselName

class DivExt(implicit val coredef: CoreDef) extends Bundle {
  val r = UInt((coredef.XLEN*2).W) // Dividend
  val d = UInt(coredef.XLEN.W) // Shifted divider
  val q = UInt(coredef.XLEN.W)
}

@chiselName
class Div(val ROUND_PER_STAGE: Int)(override implicit val coredef: CoreDef) extends ExecUnit(
  coredef.XLEN / ROUND_PER_STAGE,
  new DivExt
) {
  val round = RegInit(0.U(log2Ceil(ROUND_PER_STAGE).W))

  var idle = true.B

  for(r <- this.current) {
    idle = idle && r.pipe.instr.vacant
  }

  when(!idle) {
    round := round + 1.U
  }

  when(io.flush) {
    round := 0.U
  }

  val stall = !idle && !round.andR()

  def map(stage: Int, pipe: PipeInstr, _ext: Option[DivExt]): (DivExt, Bool) = {
    if(stage == 0) {
      val init = Wire(new DivExt).suggestName("init")
      val op1s = Wire(SInt(coredef.XLEN.W))
      val op2s = Wire(SInt(coredef.XLEN.W))

      val isDWord = (
        pipe.instr.instr.op === Decoder.Op("OP-IMM").ident
        || pipe.instr.instr.op === Decoder.Op("OP").ident
      )

      val isUnsigned = (
        pipe.instr.instr.funct3 === Decoder.MULDIV_FUNC("DIVU")
        || pipe.instr.instr.funct3 === Decoder.MULDIV_FUNC("REMU")
      )

      when(isDWord) {
        op1s := pipe.rs1val.asSInt
        op2s := pipe.rs2val.asSInt
      }.elsewhen(isUnsigned) { // UW, do not sign-extend
        op1s := (0.U(32.W) ## pipe.rs1val(31, 0)).asSInt
        op2s := (0.U(32.W) ## pipe.rs2val(31, 0)).asSInt
      }.otherwise {
        op1s := pipe.rs1val(31, 0).asSInt
        op2s := pipe.rs2val(31, 0).asSInt
      }

      when(
        isUnsigned
      ) { // Unsigned
        init.r := op1s.asUInt()
        init.d := op2s.asUInt()
        init.q := 0.U
      }.otherwise { // Signed
        init.q := 0.U
        when(op1s < 0.S) {
          init.r := (-op1s).asUInt()
        }.otherwise {
          init.r := op1s.asUInt()
        }

        when(op2s < 0.S) {
          init.d := (-op2s).asUInt
        }.otherwise {
          init.d := op2s.asUInt
        }
      }

      return (init, false.B)
    }

    val ext = _ext.get
    val nExt = Wire(new DivExt)
    nExt.d := ext.d

    val shift = ((this.DEPTH - stage + 1) * ROUND_PER_STAGE - 1).U - round
    val shifted = ext.d << shift

    when(ext.r(coredef.XLEN*2-1) && ((stage != 1).B || round =/= 0.U)) { // Prev is negative
      nExt.r := ext.r + shifted
    }.otherwise {
      nExt.r := ext.r - shifted
    }

    nExt.q := ext.q ## (!nExt.r(coredef.XLEN*2-1))

    /*
    printf(p"[DIV   ]: After stage ${stage} @ ${round}\n")
    printf(p"[DIV   ]:   q: ${Hexadecimal(nExt.q)}\n")
    printf(p"[DIV   ]:   r: ${Hexadecimal(nExt.r)}\n")
    */

    (nExt, stall)
  }

  def finalize(pipe: PipeInstr, ext: DivExt): RetireInfo = {
    val op1s = Wire(SInt(coredef.XLEN.W))
    val op2s = Wire(SInt(coredef.XLEN.W))

    val isDWord = (
      pipe.instr.instr.op === Decoder.Op("OP-IMM").ident
      || pipe.instr.instr.op === Decoder.Op("OP").ident
    )

    when(isDWord) {
      op1s := pipe.rs1val.asSInt
      op2s := pipe.rs2val.asSInt
    }.otherwise {
      op1s := pipe.rs1val(31, 0).asSInt
      op2s := pipe.rs2val(31, 0).asSInt
    }

    val qneg = Wire(Bool())
    val rneg = Wire(Bool())
    when(
      pipe.instr.instr.funct3 === Decoder.MULDIV_FUNC("DIVU")
      || pipe.instr.instr.funct3 === Decoder.MULDIV_FUNC("REMU")
    ) { // Unsigned
      qneg := false.B
      rneg := false.B
    }.otherwise {
      qneg := (op1s(coredef.XLEN-1) ^ op2s(coredef.XLEN-1)) && op2s.asUInt().orR()
      rneg := op1s(coredef.XLEN-1)
    }

    val q = ext.q
    val r = Wire(UInt())
    r := ext.r

    when(!q(0)) {
      // Negative reminder, add back divider
      r := ext.r + ext.d
    }

    val sq = Wire(SInt(coredef.XLEN.W))
    val sr = Wire(SInt(coredef.XLEN.W))
    val fq = Wire(SInt(coredef.XLEN.W))
    val fr = Wire(SInt(coredef.XLEN.W))

    when(qneg) {
      sq := -q.asSInt()
    }.otherwise {
      sq := q.asSInt()
    }

    when(rneg) {
      sr := -r.asSInt()
    }.otherwise {
      sr := r.asSInt()
    }

    when(isDWord) {
      fq := sq
      fr := sr
    }.otherwise {
      fq := sq(31, 0).asSInt
      fr := sr(31, 0).asSInt
    }

    /*
    when(!io.stall) {
      printf(p"[DIV   ]: Finalized: q = ${Hexadecimal(fq)}, r = ${Hexadecimal(fr)}\n")
    }
    */

    val info = WireDefault(RetireInfo.vacant)

    val extended = Wire(SInt(coredef.XLEN.W))
    info.wb := extended.asUInt()

    when(
      pipe.instr.instr.funct3 === Decoder.MULDIV_FUNC("DIV")
      || pipe.instr.instr.funct3 === Decoder.MULDIV_FUNC("DIVU")
    ) {
      extended := fq
    }.otherwise {
      extended := fr
    }

    info
  }

  init()
}
