package exec.units

import chisel3._
import chisel3.util._
import instr.Decoder
import exec._
import _root_.core.CoreDef

class DivExt(implicit val coredef: CoreDef) extends Bundle {
  val r = UInt((coredef.XLEN*2).W) // Dividend
  val d = UInt(coredef.XLEN.W) // Shifted divider
  val q = UInt(coredef.XLEN.W)
}

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
      val init = Wire(new DivExt)
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

      val op1 = op1s.asUInt()
      val op2 = op2s.asUInt()

      when(
        pipe.instr.instr.funct3 === Decoder.MULDIV_FUNC("DIVU")
        && pipe.instr.instr.funct3 === Decoder.MULDIV_FUNC("REMU")
      ) { // Unsigned
        init.r := op1
        init.d := op2
        init.q := 0.U
      }.otherwise { // Signed
        val sop1 = op1.asSInt
        val sop2 = op2.asSInt

        init.q := 0.U
        when(sop1(coredef.XLEN-1)) {
          init.r := (-sop1).asUInt
        }.otherwise {
          init.r := op1
        }

        when(sop2(coredef.XLEN-1)) {
          init.d := (-sop2).asUInt
        }.otherwise {
          init.d := op2.asUInt
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

    val fq = Wire(SInt(coredef.XLEN.W))
    val fr = Wire(SInt(coredef.XLEN.W))

    val qneg = Wire(Bool())
    val rneg = Wire(Bool())
    when(
      pipe.instr.instr.funct3 === Decoder.MULDIV_FUNC("DIVU")
      && pipe.instr.instr.funct3 === Decoder.MULDIV_FUNC("REMU")
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

    val hq = Wire(SInt(coredef.XLEN.W))
    val hr = Wire(SInt(coredef.XLEN.W))

    when(isDWord) {
      hq := q.asSInt
      hr := r.asSInt
    }.otherwise {
      hq := q(31, 0).asSInt
      hr := r(31, 0).asSInt
    }

    when(qneg) {
      fq := (-hq)
    }.otherwise {
      fq := hq
    }

    when(rneg) {
      fr := (-hr)
    }.otherwise {
      fr := hr
    }

    /*
    when(!io.stall) {
      printf(p"[DIV   ]: Finalized: q = ${Hexadecimal(fq)}, r = ${Hexadecimal(fr)}\n")
    }
    */

    val info = Wire(new RetireInfo)
    info.branch.nofire()
    info.mem.noop()

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
