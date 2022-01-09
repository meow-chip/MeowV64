package meowv64.exec.units

import chisel3._
import chisel3.util._
import meowv64.core.CoreDef
import meowv64.core.ExReq
import meowv64.core.ExType
import meowv64.core.PrivLevel
import meowv64.core.Status
import meowv64.exec._
import meowv64.instr.Decoder
import meowv64.instr.Decoder.InstrType

class BranchExt extends Bundle {

  /** whether the branch is taken */
  val branchTaken = Bool()

  val ex = ExReq()
  val exType = ExType()
}

class Branch(override implicit val coredef: CoreDef)
    extends ExecUnit(0, new BranchExt)
    with WithPrivPort
    with WithStatus {
  val priv = IO(Input(PrivLevel()))
  val status = IO(Input(new Status))

  def map(
      stage: Int,
      pipe: PipeInstr,
      ext: Option[BranchExt]
  ): (BranchExt, Bool) = {
    val ext = Wire(new BranchExt)
    ext.branchTaken := false.B
    ext.ex := ExReq.none
    ext.exType := DontCare

    val op1 = pipe.rs1val
    val op2 = pipe.rs2val

    when(pipe.instr.instr.op === Decoder.Op("BRANCH").ident) {
      switch(pipe.instr.instr.funct3) {
        is(Decoder.BRANCH_FUNC("BEQ")) {
          ext.branchTaken := op1 === op2
        }

        is(Decoder.BRANCH_FUNC("BNE")) {
          ext.branchTaken := op1 =/= op2
        }

        is(Decoder.BRANCH_FUNC("BLT")) {
          ext.branchTaken := op1.asSInt < op2.asSInt
        }

        is(Decoder.BRANCH_FUNC("BGE")) {
          ext.branchTaken := op1.asSInt >= op2.asSInt
        }

        is(Decoder.BRANCH_FUNC("BLTU")) {
          ext.branchTaken := op1 < op2
        }

        is(Decoder.BRANCH_FUNC("BGEU")) {
          ext.branchTaken := op1 >= op2
        }
      }
    }.elsewhen(pipe.instr.instr.op === Decoder.Op("SYSTEM").ident) { // ECALL...
      when(pipe.instr.instr.funct7 === Decoder.PRIV_FUNCT7("SFENCE.VMA")) {
        when(priv =/= PrivLevel.M && status.tvm) {
          ext.ex := ExReq.ex
          ext.exType := ExType.ILLEGAL_INSTR
        }

        // Currently does nothing
      }.otherwise {
        switch(pipe.instr.instr.rs2) {
          is(Decoder.PRIV_RS2("WFI")) {
            when(priv =/= PrivLevel.M && status.tw) {
              ext.ex := ExReq.ex
              ext.exType := ExType.ILLEGAL_INSTR
            }

            // No-op
          }

          is(Decoder.PRIV_RS2("ECALL")) {
            ext.ex := ExReq.ex
            ext.exType := Mux1H(
              Seq(
                (priv === PrivLevel.M) -> ExType.M_CALL,
                (priv === PrivLevel.S) -> ExType.S_CALL,
                (priv === PrivLevel.U) -> ExType.U_CALL
              )
            )
          }

          is(Decoder.PRIV_RS2("EBREAK")) {
            ext.ex := ExReq.ex
            ext.exType := ExType.BREAKPOINT
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
              ext.exType := ExType.ILLEGAL_INSTR
            }.elsewhen(priv === PrivLevel.S && t === ExReq.mret) {
              ext.ex := ExReq.ex
              ext.exType := ExType.ILLEGAL_INSTR
            }.elsewhen(priv === PrivLevel.S && status.tsr) {
              ext.ex := ExReq.ex
              ext.exType := ExType.ILLEGAL_INSTR
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

    when(ext.ex === ExReq.ex) {
      // info.regWaddr := 0.U
      // info.wb := 0.U // tval
      info.branch.ex(ext.exType)
    }.elsewhen(ext.ex =/= ExReq.none) {
      // info.regWaddr := 0.U
      info.branch.ret(ext.ex)
    }.elsewhen(pipe.instr.instr.op === Decoder.Op("SYSTEM").ident) {
      when(pipe.instr.instr.funct7 === Decoder.PRIV_FUNCT7("SFENCE.VMA")) {
        info.branch.sfence(pipe.instr.addr +% 4.U)
      }.otherwise {
        info.branch.nofire
      }
    }.elsewhen(pipe.instr.instr.op === Decoder.Op("BRANCH").ident) {
      // info.regWaddr := 0.U
      info.branchTaken := ext.branchTaken
      when(ext.branchTaken =/= pipe.instr.taken) {
        // mis-predict
        // branch to actual address
        val target = Wire(UInt(coredef.XLEN.W))
        when(ext.branchTaken) {
          // addr + imm
          target := (pipe.instr.instr.imm + pipe.instr.addr.asSInt).asUInt
        } otherwise {
          // next pc
          target := pipe.instr.npc
        }
        info.branch.fire(target)
      }.otherwise {
        info.branch.nofire
      }
    }.otherwise { // JAL/JALR, JAL is now in Bypass, so this must be JALR
      val linked = Wire(UInt(coredef.XLEN.W))
      linked := pipe.instr.addr + 4.U
      when(pipe.instr.instr.base === InstrType.C) {
        linked := pipe.instr.addr + 2.U // This is an compressed instr instead
      }

      // info.regWaddr := pipe.instr.instr.rd
      info.wb := linked.asUInt

      // actual branch target
      val dest =
        (((pipe.rs1val.asSInt + pipe.instr.instr.imm) >> 1) << 1).asUInt

      when(pipe.instr.taken && dest === pipe.instr.predTarget) {
        // predicted to be taken and destination is correctly predicted
        info.branch.nofire
      }.otherwise {
        // mis-predict
        info.branch.fire(dest)
      }
    }

    info
  }

  init()
}
