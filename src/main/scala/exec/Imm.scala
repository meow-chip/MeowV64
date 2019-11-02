package exec

import chisel3._
import chisel3.util._
import instr.Decoder

class ImmExt(val XLEN: Int) extends Bundle {
  val acc = UInt(XLEN.W)
}

class Imm(ADDR_WIDTH: Int, XLEN: Int) extends ExecUnit(0, new ImmExt(XLEN), ADDR_WIDTH, XLEN) {
  def map(stage: Int, pipe: PipeInstr, ext: Option[ImmExt]): (ImmExt, chisel3.Bool) = {
    val ext = Wire(new ImmExt(XLEN))
    ext.acc := DontCare

    switch(pipe.instr.instr.op) {
      is(Decoder.Op("LUI").ident) {
        val extended = Wire(SInt(64.W))
        extended := pipe.instr.instr.imm
        ext.acc := extended.asUInt
      }

      is(Decoder.Op("AUIPC").ident) {
        val result = Wire(SInt(64.W))
        result := pipe.instr.instr.imm + pipe.instr.addr.asSInt
        ext.acc := result.asUInt
        // printf(p"AUIPC Written: ${Hexadecimal(result)}\n")
      }
    }

    (ext, false.B)
  }
  def finalize(pipe: PipeInstr, ext: ImmExt): RetireInfo = {
    val info = Wire(new RetireInfo(ADDR_WIDTH, XLEN))
    info.branch.nofire()
    info.regWaddr := pipe.instr.instr.rd
    info.regWdata := ext.acc

    info
  }

  init()
}
