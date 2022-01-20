package meowv64.exec.units

import chisel3._
import hardfloat.DivSqrtRecFN_small
import hardfloat.RecFNToRecFN
import hardfloat.fNFromRecFN
import hardfloat.recFNFromFN
import meowv64.core.CoreDef
import meowv64.core.FloatD
import meowv64.core.FloatS
import meowv64.exec._
import meowv64.instr.Decoder

class FDivSqrtExt(implicit val coredef: CoreDef) extends Bundle {
  // result
  val res = UInt(coredef.XLEN.W)
  val fflags = UInt(5.W)
}

class FDivSqrt(override implicit val coredef: CoreDef)
    extends ExecUnit(
      1,
      new FDivSqrtExt
    ) {

  val idle = RegInit(true.B)
  // if flush occurred when there are inflight instructions
  // the result will be silently ignored by UnitSel
  // and the unit will wait for last instruction to complete

  def single2double(n: UInt) = {
    val convS2D = Module(
      new RecFNToRecFN(FloatS.exp, FloatS.sig, FloatD.exp, FloatD.sig)
    )
    convS2D.io.in := n
    convS2D.io.detectTininess := false.B
    convS2D.io.roundingMode := 0.U
    convS2D.io.out
  }

  def double2single(n: UInt) = {
    val convD2S = Module(
      new RecFNToRecFN(FloatD.exp, FloatD.sig, FloatS.exp, FloatS.sig)
    )
    convD2S.io.in := n
    convD2S.io.detectTininess := false.B
    convD2S.io.roundingMode := 0.U
    convD2S.io.out
  }

  val floatType = FloatD
  val div_sqrt = Module(
    new DivSqrtRecFN_small(
      floatType.exp(),
      floatType.sig(),
      0
    )
  )
  // default wiring
  div_sqrt.io.inValid := false.B
  div_sqrt.io.a := 0.U
  div_sqrt.io.b := 0.U
  div_sqrt.io.detectTininess := 0.U
  div_sqrt.io.sqrtOp := false.B
  div_sqrt.io.roundingMode := 0.U

  def map(
      stage: Int,
      pipe: PipeInstr,
      ext: Option[FDivSqrtExt]
  ): (FDivSqrtExt, Bool) = {
    val state = Wire(new FDivSqrtExt)
    state := DontCare

    // stalls
    // pipeline flow if: 1. idle 2. output is valid
    val outValid = div_sqrt.io.outValid_div || div_sqrt.io.outValid_sqrt
    val flow = idle || outValid
    val stall = ~flow
    val fire = io.next.instr.valid && flow && div_sqrt.io.inReady

    if (stage == 0) {
      // stage 0: Input
      // convert to hardfloat
      val floatType = FloatD
      val rs1valHF = WireInit(
        recFNFromFN(floatType.exp, floatType.sig, pipe.rs1val)
      )
      val rs2valHF = WireInit(
        recFNFromFN(floatType.exp, floatType.sig, pipe.rs2val)
      )

      // convert single to double
      when(pipe.instr.instr.fmt === FloatS.fmt) {
        rs1valHF := single2double(
          recFNFromFN(FloatS.exp, FloatS.sig, pipe.rs1val(31, 0))
        )
        rs2valHF := single2double(
          recFNFromFN(FloatS.exp, FloatS.sig, pipe.rs2val(31, 0))
        )
      }

      div_sqrt.io.a := rs1valHF
      div_sqrt.io.b := rs2valHF

      div_sqrt.io.inValid := fire
      when(fire) {
        idle := false.B
      }
      div_sqrt.io.roundingMode := false.B
      div_sqrt.io.detectTininess := false.B
      div_sqrt.io.sqrtOp := pipe.instr.instr.funct5 === Decoder.FP_FUNC("FSQRT")
    } else {
      // stage 1: Output
      when(pipe.instr.instr.fmt === FloatS.fmt) {
        // convert double to single and NaN-box
        state.res := FloatS.box(
          fNFromRecFN(
            FloatS.exp,
            FloatS.sig,
            double2single(div_sqrt.io.out)
          ),
          coredef.XLEN
        )
      }.otherwise {
        state.res := fNFromRecFN(floatType.exp, floatType.sig, div_sqrt.io.out)
      }
      state.fflags := div_sqrt.io.exceptionFlags
      when(outValid && ~fire) {
        idle := true.B
      }
    }
    (state, stall)
  }

  def finalize(pipe: PipeInstr, ext: FDivSqrtExt): RetireInfo = {
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
