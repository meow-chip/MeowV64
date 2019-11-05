package exec

import chisel3._
import chisel3.util._
import instr.Decoder

class DivExt(val RLEN: Int) extends Bundle {
  val r = UInt((RLEN*2).W) // Dividend
  val d = UInt(RLEN.W) // Shifted divider
  val q = UInt(RLEN.W)
}

class Div(ADDR_WIDTH: Int, XLEN: Int, HALF_WIDTH: Boolean, ROUND_PER_STAGE: Int) extends ExecUnit(
  if(HALF_WIDTH) XLEN / 2 / ROUND_PER_STAGE else XLEN / ROUND_PER_STAGE,
  new DivExt(if(HALF_WIDTH) XLEN / 2 else XLEN),
  ADDR_WIDTH,
  XLEN
) {
  val RLEN = if(HALF_WIDTH) XLEN / 2 else XLEN

  val round = RegInit(0.U(log2Ceil(ROUND_PER_STAGE).W))

  var idle = true.B

  for(r <- this.current) {
    idle = idle && r.pipe.instr.vacant
  }

  when(!idle) {
    round := round + 1.U
  }

  val stall = !idle && !round.andR()

  def map(stage: Int, pipe: PipeInstr, _ext: Option[DivExt]): (DivExt, Bool) = {
    if(stage == 0) {
      val init = Wire(new DivExt(RLEN))
      val op1 = pipe.rs1val(RLEN-1, 0)
      val op2 = pipe.rs2val(RLEN-1, 0)

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
        when(sop1(RLEN-1)) {
          init.r := (-sop1).asUInt
        }.otherwise {
          init.r := op1
        }

        when(sop2(RLEN-1)) {
          init.d := (-sop2).asUInt
        }.otherwise {
          init.d := op2.asUInt
        }
      }

      return (init, false.B)
    }

    val ext = _ext.get
    val nExt = Wire(new DivExt(RLEN))
    nExt.d := ext.d

    val shift = ((this.DEPTH - stage + 1) * ROUND_PER_STAGE - 1).U - round
    val shifted = ext.d << shift

    when(ext.r(RLEN*2-1) && ((stage != 1).B || round =/= 0.U)) { // Prev is negative
      nExt.r := ext.r + shifted
    }.otherwise {
      nExt.r := ext.r - shifted
    }

    nExt.q := ext.q ## (!nExt.r(RLEN*2-1))

    /*
    printf(p"[DIV   ]: After stage ${stage} @ ${round}\n")
    printf(p"[DIV   ]:   q: ${Hexadecimal(nExt.q)}\n")
    printf(p"[DIV   ]:   r: ${Hexadecimal(nExt.r)}\n")
    */

    (nExt, stall)
  }

  def finalize(pipe: PipeInstr, ext: DivExt): RetireInfo = {
    val fq = Wire(UInt(RLEN.W))
    val fr = Wire(UInt(RLEN.W))

    val qneg = Wire(Bool())
    val rneg = Wire(Bool())
    when(
      pipe.instr.instr.funct3 === Decoder.MULDIV_FUNC("DIVU")
      && pipe.instr.instr.funct3 === Decoder.MULDIV_FUNC("REMU")
    ) { // Unsigned
      qneg := false.B
      rneg := false.B
    }.otherwise {
      qneg := (pipe.rs1val(RLEN-1) ^ pipe.rs2val(RLEN-1)) && pipe.rs2val(RLEN-1, 0).orR()
      rneg := pipe.rs1val(RLEN-1)
    }

    val q = ext.q
    val r = Wire(UInt())
    r := ext.r

    when(!q(0)) {
      // Negative reminder, add back divider
      r := ext.r + ext.d
    }

    when(qneg) {
      fq := (-q.asSInt).asUInt
    }.otherwise {
      fq := q
    }

    when(rneg) {
      fr := (-r.asSInt).asUInt
    }.otherwise {
      fr := r
    }

    /*
    when(!io.stall) {
      printf(p"[DIV   ]: Finalized: q = ${Hexadecimal(fq)}, r = ${Hexadecimal(fr)}\n")
    }
    */

    val info = Wire(new RetireInfo(ADDR_WIDTH, XLEN))
    info.branch.nofire()
    info.regWaddr := pipe.instr.instr.rd
    val extended = Wire(SInt(XLEN.W))
    info.regWdata := extended.asUInt()

    when(
      pipe.instr.instr.funct3 === Decoder.MULDIV_FUNC("DIV")
      || pipe.instr.instr.funct3 === Decoder.MULDIV_FUNC("DIVU")
    ) {
      extended := fq.asSInt
    }.otherwise {
      extended := fr.asSInt
    }

    info
  }

  init()
}
