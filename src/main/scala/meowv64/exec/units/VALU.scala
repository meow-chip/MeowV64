package meowv64.exec.units

import chisel3._
import chisel3.util._
import meowv64.core.CoreDef
import meowv64.exec._
import meowv64.instr.Decoder

class VALUExt(implicit val coredef: CoreDef) extends Bundle {
  val acc = UInt(coredef.VLEN.W)
}

class VALU(override implicit val coredef: CoreDef)
    extends ExecUnit(0, new VALUExt) {
  def map(
      stage: Int,
      pipe: PipeInstr,
      ext: Option[VALUExt]
  ): (VALUExt, Bool) = {
    val ext = Wire(new VALUExt)
    val acc = Wire(SInt(coredef.VLEN.W))
    acc := DontCare

    (ext, false.B)
  }

  def finalize(pipe: PipeInstr, ext: VALUExt): RetireInfo = {
    val info = WireDefault(RetireInfo.vacant)
    info.wb := ext.acc.asUInt

    info
  }

  init()
}
