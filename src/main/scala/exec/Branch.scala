package exec 

import chisel3._
import chisel3.util._
import instr.Decoder
import instr.Decoder.InstrType

class BranchExt extends Bundle {
  val branched = Bool()
}

class Branch(ADDR_WIDTH: Int, XLEN: Int) extends ExecUnit(0, new BranchExt, ADDR_WIDTH, XLEN) {
  def map(stage: Int, pipe: PipeInstr, ext: Option[BranchExt]): (BranchExt, Bool) = {
    val ext = Wire(new BranchExt)
    ext.branched := false.B

    val op1 = pipe.rs1val
    val op2 = pipe.rs2val

    switch(pipe.instr.instr.funct3) {
      is(Decoder.BRANCH_FUNC("BEQ")) {
        ext.branched := op1 === op2
      }

      is(Decoder.BRANCH_FUNC("BNE")) {
        ext.branched := op1 =/= op2
      }

      is(Decoder.BRANCH_FUNC("BLT")) {
        ext.branched := op1.asSInt < op2.asSInt
      }

      is(Decoder.BRANCH_FUNC("BGE")) {
        ext.branched := op1.asSInt > op2.asSInt
      }

      is(Decoder.BRANCH_FUNC("BLTU")) {
        ext.branched := op1 < op2
      }

      is(Decoder.BRANCH_FUNC("BGEU")) {
        ext.branched := op1 > op2
      }
    }

    (ext, false.B)
  }

  def finalize(pipe: PipeInstr, ext: BranchExt): RetireInfo = {
    val info = Wire(new RetireInfo(ADDR_WIDTH, XLEN))

    when(pipe.instr.instr.op === Decoder.Op("BRANCH").ident) {
      info.regWaddr := false.B
      info.regWdata := DontCare
      when(ext.branched) {
        val target = pipe.instr.instr.imm + pipe.rs1val.asSInt
        info.branch.fire(target.asUInt)
      }.otherwise {
        info.branch.nofire()
      }
    }.otherwise { // JAL/JALR
      val linked = Wire(UInt(ADDR_WIDTH.W))
      linked := pipe.instr.addr + 4.U
      when(pipe.instr.instr.base === InstrType.toInt(InstrType.C)) {
        linked := pipe.instr.addr + 2.U // This is an compressed instr instead
      }

      info.regWaddr := pipe.instr.instr.rd
      info.regWdata := linked.asUInt

      val dest = Wire(UInt(ADDR_WIDTH.W))
      when(pipe.instr.instr.op === Decoder.Op("JAL").ident) {
        dest := pipe.instr.instr.imm + pipe.instr.addr.asSInt
      }.otherwise { // JALR
        dest := ((pipe.rs1val.asSInt + pipe.instr.instr.imm) >> 1) << 1
      }

      info.branch.fire(dest)
    }

    info
  }
}
