package exec.units

import chisel3._
import chisel3.util._
import instr.Decoder
import instr.Decoder.InstrType
import _root_.core.ExType
import _root_.core.ExReq
import exec._
import _root_.core.CoreDef
import _root_.core.PrivLevel
import _root_.core.Status

class BranchExt extends Bundle {
  val branched = Bool()

  val ex = ExReq()
  val extype = ExType()
}

class Branch(override implicit val coredef: CoreDef)
  extends ExecUnit(0, new BranchExt) with WithPrivPort with WithStatus
{
  val priv = IO(Input(PrivLevel()))
  val status = IO(Input(new Status))

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
      when(pipe.instr.instr.funct7 === Decoder.PRIV_FUNCT7("SFENCE.VMA")) {
        when(priv =/= PrivLevel.M && status.tvm) {
          ext.ex := ExReq.ex
          ext.extype := ExType.ILLEGAL_INSTR
        }

        // Currently does nothing
      }.otherwise {
        switch(pipe.instr.instr.rs2) {
          is(Decoder.PRIV_RS2("WFI")) {
            when(priv =/= PrivLevel.M && status.tw) {
              ext.ex := ExReq.ex
              ext.extype := ExType.ILLEGAL_INSTR
            }

            // No-op
          }

          is(Decoder.PRIV_RS2("ECALL")) {
            ext.ex := ExReq.ex
            ext.extype := Mux1H(Seq(
              (priv === PrivLevel.M) -> ExType.M_CALL,
              (priv === PrivLevel.S) -> ExType.S_CALL,
              (priv === PrivLevel.U) -> ExType.U_CALL
            ))
          }

          is(Decoder.PRIV_RS2("EBREAK")) {
            ext.ex := ExReq.ex
            ext.extype := ExType.BREAKPOINT
          }

          is(Decoder.PRIV_RS2("RET")) {
            val t = MuxLookup(
              pipe.instr.instr.funct7,
              ExReq.ex,
              Seq(
                Integer.parseInt("0011000", 2).U -> ExReq.mret,
                Integer.parseInt("0001000", 2).U -> ExReq.sret
              )
            )

            when(priv === PrivLevel.U) {
              ext.ex := ExReq.ex
              ext.extype := ExType.ILLEGAL_INSTR
            }.elsewhen(priv === PrivLevel.S && t === ExReq.mret) {
              ext.ex := ExReq.ex
              ext.extype := ExType.ILLEGAL_INSTR
            }.elsewhen(priv === PrivLevel.S && status.tsr) {
              ext.ex := ExReq.ex
              ext.extype := ExType.ILLEGAL_INSTR
            }.otherwise {
              ext.ex := t
            }
          }
        }
      }
    }

    (ext, false.B)
  }

  def finalize(pipe: PipeInstr, ext: BranchExt): RetireInfo = {
    val info = WireDefault(RetireInfo.vacant)

    when(ext.ex === ExReq.mret) {
      // info.regWaddr := 0.U
      info.branch.mret()
    }.elsewhen(ext.ex === ExReq.ex) {
      // info.regWaddr := 0.U
      // info.wb := 0.U // tval
      info.branch.ex(ext.extype)
    }.elsewhen(pipe.instr.instr.op === Decoder.Op("SYSTEM").ident) {
      info.branch.nofire()
    }.elsewhen(pipe.instr.instr.op === Decoder.Op("BRANCH").ident) {
      // info.regWaddr := 0.U
      when(ext.branched) {
        val target = pipe.instr.instr.imm + pipe.instr.addr.asSInt
        info.branch.fire(target.asUInt)
      }.otherwise {
        info.branch.nofire()
      }
    }.otherwise { // JAL/JALR, JAL is now in Bypass, so this must be JALR
      val linked = Wire(UInt(coredef.XLEN.W))
      linked := pipe.instr.addr + 4.U
      when(pipe.instr.instr.base === InstrType.C) {
        linked := pipe.instr.addr + 2.U // This is an compressed instr instead
      }

      // info.regWaddr := pipe.instr.instr.rd
      info.wb := linked.asUInt

      val dest = ((pipe.rs1val.asSInt + pipe.instr.instr.imm) >> 1) << 1

      info.branch.fire(dest.asUInt)
    }

    info
  }

  init()
}
