package meowv64.exec.units

import chisel3._
import hardfloat.recFNFromFN
import meowv64.core.CoreDef
import meowv64.exec._
import meowv64.core.FloatD
import hardfloat.DivSqrtRecFN_small
import hardfloat.fNFromRecFN
import meowv64.instr.Decoder
import meowv64.core.FloatS
import hardfloat.RecFNToRecFN

class FDivSqrtExt(implicit val coredef: CoreDef) extends Bundle {
  // result
  val res = UInt(coredef.XLEN.W)
  val fflags = UInt(5.W)
}

class FDivSqrt(override implicit val coredef: CoreDef)
    extends ExecUnit(
      0,
      new FDivSqrtExt
    ) {

  val idle = RegInit(true.B)
  // TODO: handle flush when there are inflight instructions
  when(~idle) {
    assert(~io.flush)
  }

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

  def map(
      stage: Int,
      pipe: PipeInstr,
      ext: Option[FDivSqrtExt]
  ): (FDivSqrtExt, Bool) = {
    val state = Wire(new FDivSqrtExt)
    state := DontCare

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

    val div_sqrt = Module(
      new DivSqrtRecFN_small(
        floatType.exp(),
        floatType.sig(),
        0
      )
    )
    div_sqrt.io.a := rs1valHF
    div_sqrt.io.b := rs2valHF
    val fire = pipe.instr.valid && idle && div_sqrt.io.inReady
    div_sqrt.io.inValid := fire
    when(fire) {
      idle := false.B
    }
    div_sqrt.io.roundingMode := false.B
    div_sqrt.io.detectTininess := false.B
    div_sqrt.io.sqrtOp := pipe.instr.instr.funct5 === Decoder.FP_FUNC("FSQRT")

    // stalls
    val outValid = div_sqrt.io.outValid_div || div_sqrt.io.outValid_sqrt
    when(outValid) {
      when(pipe.instr.instr.fmt === FloatS.fmt) {
        // convert double to single
        state.res := fNFromRecFN(
          FloatS.exp,
          FloatS.sig,
          double2single(div_sqrt.io.out)
        )
      }.otherwise {
        state.res := fNFromRecFN(floatType.exp, floatType.sig, div_sqrt.io.out)
      }
      state.fflags := div_sqrt.io.exceptionFlags
      idle := true.B
    }
    (state, ~outValid)
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
