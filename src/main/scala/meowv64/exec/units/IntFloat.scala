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

    switch(pipe.instr.instr.funct5()) {
      is(Decoder.FP_FUNC("FMV.X.D")) {
        ext.res := pipe.rs1val
      }
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
