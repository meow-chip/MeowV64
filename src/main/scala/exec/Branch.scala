package exec 

import chisel3._
import chisel3.util._
import instr.Decoder
import instr.Decoder.InstrType
import _root_.core.ExType
import _root_.core.ExReq

class BranchExt extends Bundle {
  val branched = Bool()

  val ex = ExReq()
  val extype = ExType()
}

class Branch(ADDR_WIDTH: Int, XLEN: Int) extends ExecUnit(0, new BranchExt, ADDR_WIDTH, XLEN) {
  def map(stage: Int, pipe: PipeInstr, ext: Option[BranchExt]): (BranchExt, Bool) = {
    val ext = Wire(new BranchExt)
    ext.branched := false.B
    ext.ex := ExReq.none
    ext.extype := DontCare

    val op1 = pipe.rs1val
    val op2 = pipe.rs2val

    when(pipe.instr.instr.op === Decoder.Op("BRANCH").ident) {
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
          ext.branched := op1.asSInt >= op2.asSInt
        }

        is(Decoder.BRANCH_FUNC("BLTU")) {
          ext.branched := op1 < op2
        }

        is(Decoder.BRANCH_FUNC("BGEU")) {
          ext.branched := op1 >= op2
        }
      }
    }.elsewhen(pipe.instr.instr.op === Decoder.Op("SYSTEM").ident) { // ECALL...
      switch(pipe.instr.instr.rs2) {
        is(Decoder.PRIV_RS2("ECALL")) {
          ext.ex := ExReq.ex
          ext.extype := ExType.M_CALL
        }

        is(Decoder.PRIV_RS2("EBREAK")) {
          ext.ex := ExReq.ex
          ext.extype := ExType.BREAKPOINT
        }

        is(Decoder.PRIV_RS2("RET")) {
          ext.ex := ExReq.ret
        }
      }
    }

    (ext, false.B)
  }

  def finalize(pipe: PipeInstr, ext: BranchExt): RetireInfo = {
    val info = Wire(new RetireInfo(ADDR_WIDTH, XLEN))

    when(ext.ex === ExReq.ret) {
      info.regWaddr := 0.U
      info.regWdata := DontCare
      info.branch.eret()
    }.elsewhen(ext.ex === ExReq.ex) {
      info.regWaddr := 0.U
      info.regWdata := DontCare
      info.branch.ex(ext.extype)
    }.elsewhen(pipe.instr.instr.op === Decoder.Op("BRANCH").ident) {
      info.regWaddr := 0.U
      info.regWdata := DontCare
      when(ext.branched) {
        val target = pipe.instr.instr.imm + pipe.instr.addr.asSInt
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

      val dest = Wire(SInt(ADDR_WIDTH.W))
      when(pipe.instr.instr.op === Decoder.Op("JAL").ident) {
        dest := pipe.instr.instr.imm + pipe.instr.addr.asSInt
      }.otherwise { // JALR
        dest := ((pipe.rs1val.asSInt + pipe.instr.instr.imm) >> 1) << 1
      }

      info.branch.fire(dest.asUInt)
    }

    info
  }

  init()
}
