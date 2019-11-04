package exec

import chisel3._
import instr.Decoder

class DivExt(val RLEN: Int) extends Bundle {
  val r = UInt((RLEN*2).W) // Dividend
  val d = UInt(RLEN.W) // Shifted divider
  val q = UInt(RLEN.W)
  val neg = Bool() // Negative sign
}

class Div(ADDR_WIDTH: Int, XLEN: Int, HALF_WIDTH: Boolean, ROUND_PER_STAGE: Int) extends ExecUnit(
  if(HALF_WIDTH) XLEN / 2 / ROUND_PER_STAGE else XLEN / ROUND_PER_STAGE,
  new DivExt(if(HALF_WIDTH) XLEN / 2 else XLEN),
  ADDR_WIDTH,
  XLEN
) {
  val RLEN = if(HALF_WIDTH) XLEN / 2 else XLEN

  def map(stage: Int, pipe: PipeInstr, _ext: Option[DivExt]): (DivExt, Bool) = {
    var ext = if(stage == 0) {
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
        init.neg := false.B
      }.otherwise { // Signed
        val sop1 = op1.asSInt
        val sop2 = op2.asSInt

        init.q := 0.U
        init.neg := (sop1(RLEN-1) ^ sop2(RLEN-1)) && op2.orR()
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

      init
    } else {
      _ext.get
    }

    if(stage == this.DEPTH) {
      // Final stage, normalizes 
      val next = Wire(new DivExt(RLEN))
      next := ext
      when(!ext.q(0)) {
        // Negative reminder, add back divider
        next.r := ext.r + ext.d
      }
      return (next, false.B)
    }

    val fext = (0 until ROUND_PER_STAGE).foldRight(ext)((step, prev) => {
      val next = Wire(new DivExt(RLEN))
      next.neg := prev.neg
      next.d := prev.d

      val shift = (this.DEPTH - stage - 1) * ROUND_PER_STAGE + step
      val shifted = ext.d << shift

      if(shift == RLEN-1) next.r := prev.r - shifted
      else {
        when(prev.r(RLEN*2-1)) { // Prev is negative
          next.r := prev.r + shifted
        }.otherwise {
          next.r := prev.r - shifted
        }
      }

      next.q := prev.q ## (!next.r(RLEN*2-1))

      next
    })

    /*
    printf(s"[DIV   ]: After stage ${stage}\n")
    printf(p"[DIV   ]:   q: ${Hexadecimal(fext.q)}\n")
    printf(p"[DIV   ]:   r: ${Hexadecimal(fext.r)}\n")
    printf(p"[DIV   ]:   d: ${Hexadecimal(fext.d)}\n")
    */

    (fext, false.B)
  }

  def finalize(pipe: PipeInstr, ext: DivExt): RetireInfo = {
    val fq = Wire(UInt(RLEN.W))
    val fr = Wire(UInt(RLEN.W))

    when(ext.neg) {
      fq := (-ext.q.asSInt).asUInt
      fr := (-ext.r.asSInt).asUInt
    }.otherwise {
      fq := ext.q
      fr := ext.r
    }

    // printf(p"[DIV   ]: Finalized: q = ${Hexadecimal(fq)}, r = ${Hexadecimal(fr)}\n")

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
