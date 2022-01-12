package meowv64.exec.units

import chisel3._
import chisel3.util._
import meowv64.core.CoreDef
import meowv64.exec._
import meowv64.instr.Decoder
import hardfloat.MulAddRecFN_interIo
import hardfloat.recFNFromFN
import hardfloat.MulAddRecFNToRaw_preMul
import hardfloat.MulAddRecFNToRaw_postMul
import hardfloat.RoundRawFNToRecFN
import hardfloat.fNFromRecFN

class FMAExt(implicit val coredef: CoreDef) extends Bundle {
  val expWidth = 11
  val sigWidth = 53

  val toPostMul = new MulAddRecFN_interIo(expWidth, sigWidth)
  val mulAddResult = UInt((2 * sigWidth + 1).W)
  val res = UInt((expWidth + sigWidth).W)
}

/** 2 stage FMA.
  *
  * Stage 1: convert to hardfloat, preMul, mulAdd
  *
  * Stage 2: postMul, round, convert to ieee
  */
class FMA(override implicit val coredef: CoreDef)
    extends ExecUnit(
      3,
      new FMAExt
    ) {

  def map(stage: Int, pipe: PipeInstr, ext: Option[FMAExt]): (FMAExt, Bool) = {
    val expWidth = 11
    val sigWidth = 53

    val state = Wire(new FMAExt)
    if (stage == 0) {
      // step 1: collect op and operands
      val width = expWidth + sigWidth
      val widthHF = width + 1

      // a * b + c
      val a = Wire(UInt(width.W))
      val b = Wire(UInt(width.W))
      val c = Wire(UInt(width.W))

      // convert to hardfloat
      val rs1valHF = WireInit(recFNFromFN(expWidth, sigWidth, pipe.rs1val))
      val rs2valHF = WireInit(recFNFromFN(expWidth, sigWidth, pipe.rs2val))
      val oneHF = (BigInt(1) << (expWidth + sigWidth - 1)).U(widthHF.W)

      val neg = WireInit(false.B)
      val sign = WireInit(false.B)
      val op = Cat(neg, sign)
      when(
        pipe.instr.instr.funct5 === Decoder.FP_FUNC("FADD")
      ) {
        // 1 * rs1 + rs2
        a := oneHF
        b := rs1valHF
        c := rs2valHF
      }.otherwise {
        a := 0.U
        b := 0.U
        c := 0.U
      }

      // step 2: preMul
      val preMul = Module(new MulAddRecFNToRaw_preMul(expWidth, sigWidth))
      preMul.io.op := op
      preMul.io.a := a
      preMul.io.b := b
      preMul.io.c := c

      state.toPostMul := preMul.io.toPostMul

      // step 3: mul & add
      state.mulAddResult := (preMul.io.mulAddA * preMul.io.mulAddB) +& preMul.io.mulAddC

      state.res := 0.U
    } else {
      // second stage
      state := ext.get

      // step 1: post mul
      val postMul = Module(new MulAddRecFNToRaw_postMul(expWidth, sigWidth))
      postMul.io.fromPreMul := state.toPostMul
      postMul.io.mulAddResult := state.mulAddResult
      // TODO
      postMul.io.roundingMode := 0.U

      // step 2: rounding
      val round = Module(new RoundRawFNToRecFN(expWidth, sigWidth, 0))
      round.io.in := postMul.io.rawOut
      round.io.infiniteExc := false.B
      round.io.invalidExc := false.B
      round.io.detectTininess := false.B
      round.io.roundingMode := 0.U

      // step 3: convert to ieee
      state.res := fNFromRecFN(expWidth, sigWidth, round.io.out)
    }

    // never stalls
    (state, false.B)
  }

  def finalize(pipe: PipeInstr, ext: FMAExt): RetireInfo = {
    val info = WireDefault(RetireInfo.vacant)
    info.wb := ext.res.asUInt

    info
  }

  init()
}
