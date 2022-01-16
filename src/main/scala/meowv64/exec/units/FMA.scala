package meowv64.exec.units

import chisel3._
import chisel3.util._
import hardfloat.MulAddRecFNToRaw_postMul
import hardfloat.MulAddRecFNToRaw_preMul
import hardfloat.MulAddRecFN_interIo
import hardfloat.RoundRawFNToRecFN
import meowv64.core.CoreDef
import meowv64.exec._
import meowv64.instr.Decoder

class FMAExt(implicit val coredef: CoreDef) extends Bundle {
  // intermediate
  val toPostMul = MixedVec(for (float <- coredef.FLOAT_TYPES) yield {
    new MulAddRecFN_interIo(float.exp, float.sig)
  })
  val mulAddResult = MixedVec(for (float <- coredef.FLOAT_TYPES) yield {
    UInt((2 * float.sig + 1).W)
  })

  // result
  val res = UInt(coredef.XLEN.W)
  val fflags = UInt(5.W)
}

/** 1 stage FMA.
  *
  * Cycle 1: convert to hardfloat, preMul, mulAdd
  *
  * Cycle 2: postMul, round, convert to ieee
  */
class FMA(override implicit val coredef: CoreDef)
    extends ExecUnit(
      1,
      new FMAExt
    ) {

  def map(stage: Int, pipe: PipeInstr, ext: Option[FMAExt]): (FMAExt, Bool) = {
    val state = Wire(new FMAExt)
    state := DontCare

    // loop over floating point types
    for ((float, idx) <- coredef.FLOAT_TYPES.zipWithIndex) {
      when(pipe.instr.instr.fmt === float.fmt) {
        if (stage == 0) {
          // step 1: collect op and operands
          // a * b + c
          val a = WireInit(0.U(float.widthHardfloat.W))
          val b = WireInit(0.U(float.widthHardfloat.W))
          val c = WireInit(0.U(float.widthHardfloat.W))

          // convert to hardfloat
          val rs1val = float.unbox(pipe.rs1val, coredef.XLEN)
          val rs2val = float.unbox(pipe.rs2val, coredef.XLEN)
          val rs3val = float.unbox(pipe.rs3val, coredef.XLEN)
          val rs1valHF = float.toHardfloat(rs1val)
          val rs2valHF = float.toHardfloat(rs2val)
          val rs3valHF = float.toHardfloat(rs3val)
          val oneHF =
            (BigInt(1) << (float.exp + float.sig - 1)).U(float.widthHardfloat.W)

          val neg = WireInit(false.B)
          val sign = WireInit(false.B)
          val op = Cat(neg, sign)
          switch(pipe.instr.instr.op) {
            is(Decoder.Op("MADD").ident) {
              // rs1 * rs2 + rs3
              a := rs1valHF
              b := rs2valHF
              c := rs3valHF
            }
            is(Decoder.Op("MSUB").ident) {
              // rs1 * rs2 - rs3
              sign := true.B
              a := rs1valHF
              b := rs2valHF
              c := rs3valHF
            }
            is(Decoder.Op("NMSUB").ident) {
              // - (rs1 * rs2 - rs3)
              neg := true.B
              a := rs1valHF
              b := rs2valHF
              c := rs3valHF
            }
            is(Decoder.Op("NMADD").ident) {
              // - (rs1 * rs2 + rs3)
              neg := true.B
              sign := true.B
              a := rs1valHF
              b := rs2valHF
              c := rs3valHF
            }
            is(Decoder.Op("OP-FP").ident) {
              switch(pipe.instr.instr.funct5) {
                is(Decoder.FP_FUNC("FADD")) {
                  // 1 * rs1 + rs2
                  a := oneHF
                  b := rs1valHF
                  c := rs2valHF
                }
                is(Decoder.FP_FUNC("FSUB")) {
                  // 1 * rs1 - rs2
                  sign := true.B
                  a := oneHF
                  b := rs1valHF
                  c := rs2valHF
                }
                is(Decoder.FP_FUNC("FMUL")) {
                  // rs1 * rs2 + 0
                  a := rs1valHF
                  b := rs2valHF
                }
              }
            }
          }

          // step 2: preMul
          val preMul = Module(new MulAddRecFNToRaw_preMul(float.exp, float.sig))
          preMul.suggestName(s"preMul_${float.name}")
          preMul.io.op := op
          preMul.io.a := a
          preMul.io.b := b
          preMul.io.c := c

          state.toPostMul(idx) := preMul.io.toPostMul

          // step 3: mul & add
          state.mulAddResult(
            idx
          ) := (preMul.io.mulAddA * preMul.io.mulAddB) +& preMul.io.mulAddC

          state.res := 0.U
          state.fflags := 0.U
        } else {
          // second stage
          state := ext.get

          // step 1: post mul
          val postMul =
            Module(new MulAddRecFNToRaw_postMul(float.exp, float.sig))
          postMul.suggestName(s"postMul_${float.name}")
          postMul.io.fromPreMul := state.toPostMul(idx)
          postMul.io.mulAddResult := state.mulAddResult(idx)
          // TODO
          postMul.io.roundingMode := 0.U

          // step 2: rounding
          val round = Module(new RoundRawFNToRecFN(float.exp, float.sig, 0))
          round.suggestName(s"round_${float.name}")
          round.io.in := postMul.io.rawOut
          round.io.infiniteExc := false.B
          round.io.invalidExc := postMul.io.invalidExc
          round.io.detectTininess := false.B
          round.io.roundingMode := 0.U

          // step 3: convert to ieee
          state.res := float.box(
            float.fromHardfloat(round.io.out),
            coredef.XLEN
          )
          state.fflags := round.io.exceptionFlags
        }
      }
    }

    // never stalls
    (state, false.B)
  }

  def finalize(pipe: PipeInstr, ext: FMAExt): RetireInfo = {
    val info = WireDefault(RetireInfo.vacant)
    // result
    info.wb := ext.res.asUInt

    // fflags
    info.updateFFlags := true.B
    info.fflags := ext.fflags

    info
  }

  init()
}
