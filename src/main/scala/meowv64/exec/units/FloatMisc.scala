package meowv64.exec.units

import chisel3._
import chisel3.util._
import hardfloat.CompareRecFN
import hardfloat.recFNFromFN
import meowv64.core.CoreDef
import meowv64.exec._
import meowv64.instr.Decoder
import hardfloat.RecFNToIN
import hardfloat.INToRecFN
import hardfloat.fNFromRecFN
import hardfloat.RecFNToRecFN

class IntFloatExt(implicit val coredef: CoreDef) extends Bundle {
  val res = UInt(coredef.XLEN.W)

  val updateFFlags = Bool()
  val fflags = UInt(5.W)
}

/** Handles instructions: FMV.X.D, FMV.D.X, FCLASS.D, FEQ.D, FLT.D, FLE.D
  */
class FloatMisc(override implicit val coredef: CoreDef)
    extends ExecUnit(0, new IntFloatExt) {
  def map(
      stage: Int,
      pipe: PipeInstr,
      ext: Option[IntFloatExt]
  ): (IntFloatExt, Bool) = {
    val ext = Wire(new IntFloatExt)
    ext.res := 0.U
    ext.updateFFlags := false.B
    ext.fflags := 0.U

    val expWidth = 11
    val sigWidth = 53
    val singleExpWidth = 8
    val singleSigWidth = 24

    // double
    val rs1valHF = WireInit(recFNFromFN(expWidth, sigWidth, pipe.rs1val))
    val rs2valHF = WireInit(recFNFromFN(expWidth, sigWidth, pipe.rs2val))
    // single
    val rs1valSingleHF = WireInit(
      recFNFromFN(singleExpWidth, singleSigWidth, pipe.rs1val(31, 0))
    )
    val rs2valSingleHF = WireInit(
      recFNFromFN(singleExpWidth, singleSigWidth, pipe.rs2val(31, 0))
    )

    when(
      pipe.instr.instr.funct5 === Decoder.FP_FUNC(
        "FMV.X.D"
      ) && pipe.instr.instr.funct3 === 0.U
    ) {
      // fmv.x.d
      ext.res := pipe.rs1val
    }.elsewhen(
      pipe.instr.instr.funct5 === Decoder.FP_FUNC(
        "FMV.D.X"
      ) && pipe.instr.instr.funct3 === 0.U
    ) {
      // fmv.d.x
      ext.res := pipe.rs1val
    }.elsewhen(
      pipe.instr.instr.funct5 === Decoder.FP_FUNC(
        "FCLASS"
      ) && pipe.instr.instr.funct3 === 1.U
    ) {
      val sign = pipe.rs1val(expWidth + sigWidth - 1)
      val exp = pipe.rs1val(expWidth + sigWidth - 2, sigWidth - 1)
      val sig = pipe.rs1val(sigWidth - 2, 0)

      val expZero = exp === 0.U
      val expMax = exp.andR
      val isZero = expZero && sig === 0.U
      val isSubnormal = expZero && sig =/= 0.U
      val isNormal = exp.orR && ~exp.andR
      val isInf = expMax && sig === 0.U
      val isNan = expMax && sig.orR
      val isSNan = isNan && sig(sigWidth - 2) === false.B
      val isQNan = isNan && sig(sigWidth - 2) === true.B

      ext.res := Cat(
        isQNan,
        isSNan,
        ~sign && isInf,
        ~sign && isNormal,
        ~sign && isSubnormal,
        ~sign && isZero,
        sign && isZero,
        sign && isSubnormal,
        sign && isNormal,
        sign && isInf
      )
    }.elsewhen(
      pipe.instr.instr.funct5 === Decoder.FP_FUNC(
        "FCMP"
      )
    ) {
      // FEQ.D, FLT.D, FLE.D
      val cmp = Module(new CompareRecFN(expWidth, sigWidth))
      cmp.io.a := rs1valHF
      cmp.io.b := rs2valHF
      cmp.io.signaling := true.B

      when(pipe.instr.instr.funct3 === 2.U) {
        // FEQ
        ext.res := cmp.io.eq
        // do not signal qNan in feq
        cmp.io.signaling := false.B
      }.elsewhen(pipe.instr.instr.funct3 === 1.U) {
        // FLT
        ext.res := cmp.io.lt
      }.elsewhen(pipe.instr.instr.funct3 === 0.U) {
        // FLE
        ext.res := cmp.io.lt || cmp.io.eq
      }

      ext.updateFFlags := true.B
      ext.fflags := cmp.io.exceptionFlags
    }.elsewhen(
      pipe.instr.instr.funct5 === Decoder.FP_FUNC(
        "FLOAT2INT"
      )
    ) {
      // convert float to int
      val convF2I = Module(new RecFNToIN(expWidth, sigWidth, coredef.XLEN))
      convF2I.io.in := rs1valHF
      convF2I.io.signedOut := false.B
      convF2I.io.roundingMode := 0.U

      when(pipe.instr.instr.rs2 === 0.U) {
        // FCVT.W.D
        convF2I.io.signedOut := true.B
      }.elsewhen(pipe.instr.instr.rs2 === 1.U) {
        // FCVT.WU.D
      }.elsewhen(pipe.instr.instr.rs2 === 2.U) {
        // FCVT.L.D
        convF2I.io.signedOut := true.B
      }.elsewhen(pipe.instr.instr.rs2 === 3.U) {
        // FCVT.LU.D
      }.otherwise {
        assert(false.B)
      }
      ext.res := convF2I.io.out
      // see rocket chip
      ext.fflags := Cat(
        convF2I.io.intExceptionFlags(2, 1).orR,
        0.U(3.W),
        convF2I.io.intExceptionFlags(0)
      )
      ext.updateFFlags := true.B
    }.elsewhen(
      pipe.instr.instr.funct5 === Decoder.FP_FUNC(
        "INT2FLOAT"
      )
    ) {
      // convert int to float
      val convI2F = Module(new INToRecFN(coredef.XLEN, expWidth, sigWidth))
      convI2F.io.in := pipe.rs1val
      convI2F.io.signedIn := false.B
      convI2F.io.roundingMode := 0.U
      convI2F.io.detectTininess := false.B

      when(pipe.instr.instr.rs2 === 0.U) {
        // FCVT.D.W
        convI2F.io.signedIn := true.B
      }.elsewhen(pipe.instr.instr.rs2 === 1.U) {
        // FCVT.D.WU
        // clip to 32 bit
        convI2F.io.in := pipe.rs1val(31, 0)
      }.elsewhen(pipe.instr.instr.rs2 === 2.U) {
        // FCVT.D.L
        convI2F.io.signedIn := true.B
      }.elsewhen(pipe.instr.instr.rs2 === 3.U) {
        // FCVT.D.LU
      }.otherwise {
        assert(false.B)
      }
      ext.res := fNFromRecFN(expWidth, sigWidth, convI2F.io.out)
      ext.fflags := convI2F.io.exceptionFlags
      ext.updateFFlags := true.B
    }.elsewhen(
      pipe.instr.instr.funct5 === Decoder.FP_FUNC(
        "FLOAT2FLOAT"
      )
    ) {
      // convert float to float
      when(pipe.instr.instr.rs2 === 0.U) {
        // FCVT.D.S
        // single precision to double precision
        // widening
        val convS2D = Module(
          new RecFNToRecFN(singleExpWidth, singleSigWidth, expWidth, sigWidth)
        )
        convS2D.io.in := rs1valSingleHF
        convS2D.io.detectTininess := false.B
        convS2D.io.roundingMode := 0.U

        ext.res := fNFromRecFN(expWidth, sigWidth, convS2D.io.out)
        ext.fflags := convS2D.io.exceptionFlags
      }.elsewhen(pipe.instr.instr.rs2 === 1.U) {
        // FCVT.S.D
        // double precision to single precision
        val convD2S = Module(
          new RecFNToRecFN(expWidth, sigWidth, singleExpWidth, singleSigWidth)
        )

        convD2S.io.in := rs1valHF
        convD2S.io.detectTininess := false.B
        convD2S.io.roundingMode := 0.U

        // NaN boxing
        ext.res := Cat(
          "hFFFFFFFF".U,
          fNFromRecFN(singleExpWidth, singleSigWidth, convD2S.io.out)
        )
        ext.fflags := convD2S.io.exceptionFlags
      }.otherwise {
        assert(false.B)
      }
      ext.updateFFlags := true.B
    }

    (ext, false.B)
  }

  def finalize(pipe: PipeInstr, ext: IntFloatExt): RetireInfo = {
    val info = WireDefault(RetireInfo.vacant)

    // result
    info.wb := ext.res.asUInt

    // fflags
    info.updateFFlags := ext.updateFFlags
    info.fflags := ext.fflags

    info
  }

  init()
}
