package exec.units

import chisel3._
import chisel3.util._
import instr.Decoder
import cache._
import chisel3.experimental.ChiselEnum
import exec._
import _root_.core.CoreDef

class LSUExt(implicit val coredef: CoreDef) extends Bundle {
  val wdata = UInt(coredef.XLEN.W)
}

class LSU(dcopt: L1DOpts)(override implicit val coredef: CoreDef) extends ExecUnit(0, new LSUExt) {
  val reader = IO(new DCReader(dcopt))
  val writer = IO(new DCWriter(dcopt))

  reader := DontCare
  reader.read := false.B

  writer := DontCare
  writer.write := false.B

  // Load/Store related FSM
  object State extends ChiselEnum {
    val idle, waiting, hold = Value
  }
  val lsState = RegInit(State.idle)
  val lsNextState = Wire(lsState.cloneType)
  val lsAddr = Reg(UInt(coredef.ADDR_WIDTH.W))
  val holdBuf = Reg(UInt(coredef.XLEN.W))

  lsNextState := lsState
  lsState := lsNextState
  /*
  when(lsState =/= lsNextState) {
    printf(p"[LSU   ]: State transfer -> ${Hexadecimal(lsNextState)}\n")
  }
  */

  override def map(stage: Int, pipe: PipeInstr, ext: Option[LSUExt]): (LSUExt, Bool) =  {
    val ext = Wire(new LSUExt)
    val stall = Wire(Bool())
    ext.wdata := DontCare
    stall := false.B

    switch(pipe.instr.instr.op) {
      is(Decoder.Op("LOAD").ident) {
        switch(lsState) {
          is(State.idle) {
            val lsAddrCompute = (pipe.rs1val.asSInt + pipe.instr.instr.imm).asUInt
            lsAddr := lsAddrCompute
            reader.addr := (lsAddrCompute >> 3) << 3 // Align
            reader.read := true.B

            // printf("Transfered by LOAD\n")
            lsNextState := State.waiting
            stall := true.B
          }

          is(State.waiting) {
            val data = reader.data
            val shifted = data >> (lsAddr(2, 0) * 8.U)
            val result = Wire(UInt(coredef.XLEN.W))
            val signedResult = Wire(SInt(coredef.XLEN.W))

            signedResult := DontCare
            result := signedResult.asUInt
            ext.wdata := result
            holdBuf := result

            switch(pipe.instr.instr.funct3) {
              is(Decoder.LOAD_FUNC("LB")) {
                signedResult := shifted(7, 0).asSInt
              }

              is(Decoder.LOAD_FUNC("LH")) {
                signedResult := shifted(15, 0).asSInt
              }

              is(Decoder.LOAD_FUNC("LW")) {
                signedResult := shifted(31, 0).asSInt
              }

              is(Decoder.LOAD_FUNC("LD")) {
                result := shifted
              }

              is(Decoder.LOAD_FUNC("LBU")) {
                result := shifted(7, 0)
              }

              is(Decoder.LOAD_FUNC("LHU")) {
                result := shifted(16, 0)
              }

              is(Decoder.LOAD_FUNC("LWU")) {
                result := shifted(32, 0)
              }
            }

            stall := true.B

            when(!reader.stall) {
              stall := false.B
              /*
              printf(p"Load recv:\n  Rdata: ${Hexadecimal(dc.rdata)}\n")
              printf(p"Addr: ${Hexadecimal((lsAddr >> 3) << 3)}\n")
              printf(p"Shifted output: ${Hexadecimal(shifted)}\n")
              */
              // Commit

              // printf("Transfered by LOAD\n")

              when(io.pause) {
                lsNextState := State.waiting
              }.otherwise {
                lsNextState := State.idle
              }
            }
          }

          is(State.hold) {
            ext.wdata := holdBuf
            when(!io.pause) {
              // printf("Transfered by LOAD\n")
              lsNextState := State.idle
            }
          }
        }
      }

      is(Decoder.Op("STORE").ident) {
        val lsAddrCompute = (pipe.rs1val.asSInt() + pipe.instr.instr.imm).asUInt
        val lsAddrShift = lsAddrCompute(2, 0) // By 8 bits
        lsAddr := lsAddrCompute
        writer.addr := (lsAddrCompute >> 3) << 3 // Align

        val tailMask = Wire(UInt((coredef.XLEN/8).W))
        tailMask := 0.U

        /*
        printf("Store emit: \n")
        printf(p"  Addr: ${Hexadecimal(dc.addr)}\n")
        printf(p"  Wdata: ${Hexadecimal(dc.wdata)}\n")
        printf(p"  BE: ${Hexadecimal(dc.be)}\n")
        */
        writer.data := pipe.rs2val << (lsAddrShift * 8.U)
        writer.be := tailMask << lsAddrShift

        switch(pipe.instr.instr.funct3) {
          is(Decoder.STORE_FUNC("SB")) { tailMask := 0x01.U }
          is(Decoder.STORE_FUNC("SH")) { tailMask := 0x03.U }
          is(Decoder.STORE_FUNC("SW")) { tailMask := 0x0f.U }
          is(Decoder.STORE_FUNC("SD")) { tailMask := 0xff.U }
        }

        // printf("Transfered by STORE\n")
        lsNextState := State.idle

        switch(lsState) {
          is(State.idle) {
            writer.write := true.B
            stall := true.B

            when(!writer.stall) {
              when(io.pause) {
                lsNextState := State.hold
              }.otherwise {
                stall := false.B
              }
            }
          }

          is(State.hold) {
            stall := false.B
          }
        }
      }
    }

    (ext, stall)
  }
  override def finalize(pipe: PipeInstr, ext: LSUExt): RetireInfo = {
    val info = Wire(new RetireInfo)
    info.branch.nofire()

    when(pipe.instr.instr.op === Decoder.Op("LOAD").ident) {
      // info.regWaddr := pipe.instr.instr.rd
      info.wb := ext.wdata
    }.otherwise {
      // info.regWaddr := 0.U
      info.wb := DontCare
    }

    info
  }

  init()
}
