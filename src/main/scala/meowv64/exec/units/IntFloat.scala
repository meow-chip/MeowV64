package meowv64.exec.units

import chisel3._
import chisel3.util._
import meowv64.core.CoreDef
import meowv64.exec._
import meowv64.instr.Decoder

class IntFloatExt(implicit val coredef: CoreDef) extends Bundle {
  val res = UInt(coredef.XLEN.W)
  val illegal = Bool()
}

/** Handles integer <-> float.
  */
class IntFloat(override implicit val coredef: CoreDef)
    extends ExecUnit(0, new IntFloatExt) {
  def map(
      stage: Int,
      pipe: PipeInstr,
      ext: Option[IntFloatExt]
  ): (IntFloatExt, Bool) = {
    val ext = Wire(new IntFloatExt)
    ext.res := 0.U
    ext.illegal := false.B

    val expWidth = 11
    val sigWidth = 53

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
    } otherwise {
      ext.illegal := true.B
    }

    (ext, false.B)
  }

  def finalize(pipe: PipeInstr, ext: IntFloatExt): RetireInfo = {
    val info = WireDefault(RetireInfo.vacant)
    info.wb := ext.res.asUInt

    info
  }

  init()
}
