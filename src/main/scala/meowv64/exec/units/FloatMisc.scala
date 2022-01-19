package meowv64.exec.units

import chisel3._
import chisel3.util._
import hardfloat.CompareRecFN
import hardfloat.INToRecFN
import hardfloat.RecFNToIN
import hardfloat.RecFNToRecFN
import hardfloat.fNFromRecFN
import hardfloat.recFNFromFN
import meowv64.core.CoreDef
import meowv64.exec._
import meowv64.instr.Decoder
import meowv64.core.FloatS

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

    val rs1Values =
      coredef.FLOAT_TYPES.map(f => f.unbox(pipe.rs1val, coredef.XLEN))
    val rs2Values =
      coredef.FLOAT_TYPES.map(f => f.unbox(pipe.rs2val, coredef.XLEN))
    val rs1HFValues = coredef.FLOAT_TYPES
      .zip(rs1Values)
      .map({ case (f, v) => f.toHardfloat(v) })
    val rs2HFValues = coredef.FLOAT_TYPES
      .zip(rs2Values)
      .map({ case (f, v) => f.toHardfloat(v) })

    val expWidth = 11
    val sigWidth = 53
    val singleExpWidth = 8
    val singleSigWidth = 24

    // double
    val rs1valHF = WireInit(recFNFromFN(expWidth, sigWidth, pipe.rs1val))
    // single
    val rs1valSingleHF = WireInit(
      recFNFromFN(singleExpWidth, singleSigWidth, pipe.rs1val(31, 0))
    )

    when(
      pipe.instr.instr.funct5 === Decoder.FP_FUNC(
        "FMV.X.D/W"
      ) && pipe.instr.instr.funct3 === 0.U
    ) {
      when(pipe.instr.instr.funct7(1, 0) === 0.U) {
        // fmv.x.w
        // sign extension
        // do not consider nan boxing here
        ext.res := Fill(32, pipe.rs1val(31)) ## pipe.rs1val(31, 0)
      }.otherwise {
        // fmv.x.d
        ext.res := pipe.rs1val
      }
    }.elsewhen(
      pipe.instr.instr.funct5 === Decoder.FP_FUNC(
        "FMV.D/W.X"
      ) && pipe.instr.instr.funct3 === 0.U
    ) {
      when(pipe.instr.instr.funct7(1, 0) === 0.U) {
        // fmv.w.x
        // nan boxing
        ext.res := FloatS.box(pipe.rs1val(31, 0), coredef.XLEN)
      }.otherwise {
        // fmv.d.x
        ext.res := pipe.rs1val
      }
    }.elsewhen(
      pipe.instr.instr.funct5 === Decoder.FP_FUNC(
        "FCLASS"
      ) && pipe.instr.instr.funct3 === 1.U
    ) {
      for ((float, idx) <- coredef.FLOAT_TYPES.zipWithIndex) {
        when(pipe.instr.instr.fmt === float.fmt) {
          val sign = rs1Values(idx)(float.exp + float.sig - 1)
          val exp = rs1Values(idx)(float.exp + float.sig - 2, float.sig - 1)
          val sig = rs1Values(idx)(float.sig - 2, 0)

          val expZero = exp === 0.U
          val expMax = exp.andR
          val isZero = expZero && sig === 0.U
          val isSubnormal = expZero && sig =/= 0.U
          val isNormal = exp.orR && ~exp.andR
          val isInf = expMax && sig === 0.U
          val isNaN = expMax && sig.orR
          val isSNaN = isNaN && sig(float.sig - 2) === false.B
          val isQNaN = isNaN && sig(float.sig - 2) === true.B

          ext.res := Cat(
            isQNaN,
            isSNaN,
            ~sign && isInf,
            ~sign && isNormal,
            ~sign && isSubnormal,
            ~sign && isZero,
            sign && isZero,
            sign && isSubnormal,
            sign && isNormal,
            sign && isInf
          )
        }
      }
    }.elsewhen(
      pipe.instr.instr.funct5 === Decoder.FP_FUNC(
        "FCMP"
      ) || pipe.instr.instr.funct5 === Decoder.FP_FUNC(
        "FMINMAX"
      )
    ) {
      ext.fflags := DontCare

      // loop over floating point types
      for ((float, idx) <- coredef.FLOAT_TYPES.zipWithIndex) {
        when(pipe.instr.instr.fmt === float.fmt) {
          val cmp = Module(new CompareRecFN(float.exp, float.sig))
          cmp.suggestName(s"cmp_${float.name}")
          cmp.io.a := rs1HFValues(idx)
          cmp.io.b := rs2HFValues(idx)
          cmp.io.signaling := true.B

          when(
            pipe.instr.instr.funct5 === Decoder.FP_FUNC(
              "FCMP"
            )
          ) {
            // FEQ, FLT, FLE
            when(pipe.instr.instr.funct3 === 2.U) {
              // FEQ
              ext.res := cmp.io.eq
              // do not signal qNaN in feq
              cmp.io.signaling := false.B
            }.elsewhen(pipe.instr.instr.funct3 === 1.U) {
              // FLT
              ext.res := cmp.io.lt
            }.elsewhen(pipe.instr.instr.funct3 === 0.U) {
              // FLE
              ext.res := cmp.io.lt || cmp.io.eq
            }
          }.otherwise {
            // FMIN, FMAX
            cmp.io.signaling := false.B
            val retRs1 = WireInit(true.B)
            val retNaN = WireInit(false.B)

            val rs1NaN = float.isNaN(pipe.rs1val)
            val rs2NaN = float.isNaN(pipe.rs2val)

            val lt = WireInit(cmp.io.lt)
            // special handling for -0.0 and +0.0
            when(
              pipe.rs1val(float.width - 1) && ~pipe.rs2val(float.width - 1)
            ) {
              // -0.0 < +0.0
              lt := true.B
            }.elsewhen(
              ~pipe.rs1val(float.width - 1) && pipe.rs2val(float.width - 1)
            ) {
              // -0.0 > +0.0
              lt := false.B
            }

            when(pipe.instr.instr.funct3 === 0.U) {
              // FMIN
              retRs1 := lt
            }.otherwise {
              // FMAX
              retRs1 := ~lt
            }

            when(rs1NaN && ~rs2NaN) {
              // rs2 is not nan
              retRs1 := false.B
            }.elsewhen(~rs1NaN && rs2NaN) {
              // rs1 is not nan
              retRs1 := true.B
            }.elsewhen(rs1NaN && rs2NaN) {
              // return nan
              retNaN := true.B
            }

            ext.res := float.box(
              Mux(
                retNaN,
                float.nan(),
                Mux(retRs1, pipe.rs1val, pipe.rs2val)
              ),
              coredef.XLEN
            )
          }
          ext.fflags := cmp.io.exceptionFlags
        }
      }

      ext.updateFFlags := true.B
    }.elsewhen(
      pipe.instr.instr.funct5 === Decoder.FP_FUNC(
        "FLOAT2INT"
      )
    ) {
      // convert float to int
      // convert float32/float64 to int64 first
      // then clamp to int32
      for ((float, idx) <- coredef.FLOAT_TYPES.zipWithIndex) {
        when(pipe.instr.instr.fmt === float.fmt) {
          val convF2I =
            Module(new RecFNToIN(float.exp, float.sig, coredef.XLEN))
          convF2I.io.in := rs1HFValues(idx)
          convF2I.io.signedOut := false.B
          convF2I.io.roundingMode := pipe.instr.instr.funct3

          convF2I.suggestName(s"convF2I_${float.name}")

          // overflow in int64 -> int32
          val overflow = WireInit(false.B)
          when(pipe.instr.instr.rs2(1) === 0.U) {
            // FCVT.W.S/FCVT.WU.S/FCVT.W.D/FCVT.WU.D
            // convert 32/64-bit float to 32-bit int/uint
            // clamp to int32 range
            val clamped = WireInit(0.U(32.W))

            when(pipe.instr.instr.rs2(0) === 0.U) {
              // signed
              when(
                convF2I.io.out(coredef.XLEN - 1) && ~convF2I.io
                  .out(coredef.XLEN - 1, 31)
                  .andR
              ) {
                // negative underflow
                clamped := 1.U ## Fill(31, 0.U)
                overflow := true.B
              }.elsewhen(
                ~convF2I.io.out(coredef.XLEN - 1) && convF2I.io
                  .out(coredef.XLEN - 1, 31)
                  .orR
              ) {
                // positive overflow
                clamped := 0.U ## Fill(31, 1.U)
                overflow := true.B
              }.otherwise {
                clamped := convF2I.io.out(31, 0)
              }
            }.otherwise {
              // unsigned
              when(convF2I.io.out(coredef.XLEN - 1, 32).orR) {
                // positive overflow
                clamped := Fill(32, 1.U)
              }.otherwise {
                clamped := convF2I.io.out(31, 0)
              }
            }

            // sign extension
            ext.res := Fill(32, clamped(31)) ## clamped(31, 0)
          }.otherwise {
            // FCVT.L.S/FCVT.LU.S/FCVT.L.D/FCVT.LU.D
            // convert 32/64-bit float to 64-bit int/uint
            ext.res := convF2I.io.out
          }

          when(pipe.instr.instr.rs2(0) === 0.U) {
            // FCVT.W.D/FCVT.L.D/FCVT.W.S/FCVT.L.S
            // signed int
            convF2I.io.signedOut := true.B
          }

          // see rocket chip
          ext.fflags := Cat(
            convF2I.io.intExceptionFlags(2, 1).orR | overflow,
            0.U(3.W),
            convF2I.io.intExceptionFlags(0)
          )
        }
      }

      ext.updateFFlags := true.B
    }.elsewhen(
      pipe.instr.instr.funct5 === Decoder.FP_FUNC(
        "INT2FLOAT"
      )
    ) {
      // convert int to float
      // int32 is a subset of int64
      // so we convert int32 to int64 first
      // and then convert to float32/float64
      for (float <- coredef.FLOAT_TYPES) {
        when(pipe.instr.instr.fmt === float.fmt) {
          val convI2F =
            Module(new INToRecFN(coredef.XLEN, float.exp, float.sig))
          convI2F.suggestName(s"convI2F_${float.name}")
          convI2F.io.signedIn := false.B
          convI2F.io.roundingMode := 0.U
          convI2F.io.detectTininess := false.B

          convI2F.io.in := pipe.rs1val
          when(pipe.instr.instr.rs2 === 0.U) {
            // FCVT.D.W/FCVT.S.W
            // convert 32-bit int to 32/64-bit float
            convI2F.io.signedIn := true.B
            convI2F.io.in := Fill(32, pipe.rs1val(31)) ## pipe.rs1val(31, 0)
          }.elsewhen(pipe.instr.instr.rs2 === 1.U) {
            // FCVT.D.WU/FCVT.S.WU
            // convert 32-bit uint to 32/64-bit float
            convI2F.io.in := pipe.rs1val(31, 0)
          }.elsewhen(pipe.instr.instr.rs2 === 2.U) {
            // FCVT.D.L/FCVT.S.L
            // convert 64-bit int to 32/64-bit float
            convI2F.io.signedIn := true.B
          }.elsewhen(pipe.instr.instr.rs2 === 3.U) {
            // FCVT.D.LU/FCVT.S.LU
            // convert 64-bit uint to 32/64-bit float
          }.otherwise {
            assert(false.B)
          }
          ext.res := float.box(
            float.fromHardfloat(convI2F.io.out),
            coredef.XLEN
          )
          ext.fflags := convI2F.io.exceptionFlags
          ext.updateFFlags := true.B
        }
      }
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
        ext.res := FloatS.box(
          fNFromRecFN(singleExpWidth, singleSigWidth, convD2S.io.out),
          coredef.XLEN
        )
        ext.fflags := convD2S.io.exceptionFlags
      }.otherwise {
        assert(false.B)
      }
      ext.updateFFlags := true.B
    }.elsewhen(
      pipe.instr.instr.funct5 === Decoder.FP_FUNC(
        "FSGNJ"
      )
    ) {
      // sign injection
      when(pipe.instr.instr.funct7(1, 0) === 0.U) {
        val rs1Value = rs1Values(0)
        val rs2Value = rs2Values(0)
        when(pipe.instr.instr.funct3 === 0.U) {
          // FSGNJ.S
          ext.res := Fill(32, 1.U) ## rs2Value(31) ## rs1Value(30, 0)
        }.elsewhen(pipe.instr.instr.funct3 === 1.U) {
          // FSGNJN.S
          ext.res := Fill(32, 1.U) ## (~rs2Value(31)) ## rs1Value(30, 0)
        }.elsewhen(pipe.instr.instr.funct3 === 2.U) {
          // FSGNJX.S
          ext.res := Fill(32, 1.U) ##
            (rs2Value(31) ^ rs1Value(31)) ## rs1Value(30, 0)
        }
      }.otherwise {
        when(pipe.instr.instr.funct3 === 0.U) {
          // FSGNJ.D
          ext.res := pipe.rs2val(63) ## pipe.rs1val(62, 0)
        }.elsewhen(pipe.instr.instr.funct3 === 1.U) {
          // FSGNJN.D
          ext.res := (~pipe.rs2val(63)) ## pipe.rs1val(62, 0)
        }.elsewhen(pipe.instr.instr.funct3 === 2.U) {
          // FSGNJX.D
          ext.res := (pipe.rs2val(63) ^ pipe.rs1val(63)) ## pipe.rs1val(62, 0)
        }
      }
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
