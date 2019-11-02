package exec

import chisel3._
import chisel3.util._
import instr.Decoder
import cache.DCachePort
import data.AXI

class LSUExt(XLEN: Int) extends Bundle {
  val wdata = UInt(XLEN.W)
}

class LSU(ADDR_WIDTH: Int, XLEN: Int) extends ExecUnit(0, new LSUExt(XLEN), ADDR_WIDTH, XLEN) {
  val d$ = IO(Flipped(new DCachePort(ADDR_WIDTH, XLEN)))
  val axi = IO(new AXI(XLEN))

  d$ <> DontCare
  d$.axi <> axi
  d$.pause := io.pause

  d$.read := false.B
  d$.write := false.B

  // Load/Store related FSM
  val lsIDLE :: lsWAIT :: nil = Enum(2)
  val lsState = RegInit(lsIDLE)
  val lsNextState = Wire(lsState.cloneType)
  val lsAddr = Reg(UInt(ADDR_WIDTH.W))

  lsNextState := lsState
  when(lsNextState === lsWAIT || !io.pause) {
    lsState := lsNextState
  }

  override def map(stage: Int, pipe: PipeInstr, ext: Option[LSUExt]): (LSUExt, Bool) =  {
    val ext = Wire(new LSUExt(XLEN))
    ext.wdata := DontCare

    switch(pipe.instr.instr.op) {
      is(Decoder.Op("LOAD").ident) {
        switch(lsState) {
          is(lsIDLE) {
            val lsAddrCompute = (pipe.rs1val.asSInt + pipe.instr.instr.imm).asUInt
            lsAddr := lsAddrCompute
            d$.addr := (lsAddrCompute >> 3) << 3 // Align
            d$.read := true.B

            // printf("Transfered by LOAD")
            lsNextState := lsWAIT
          }

          is(lsWAIT) {
            val data = d$.rdata
            val shifted = data >> (lsAddr(2, 0) * 8.U)
            val result = Wire(UInt(XLEN.W))
            val signedResult = Wire(SInt(XLEN.W))

            signedResult := DontCare
            result := signedResult.asUInt
            ext.wdata := result

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

            when(!d$.stall) {
              /*
              printf(p"Load recv:\n  Rdata: ${Hexadecimal(d$.rdata)}\n")
              printf(p"Addr: ${Hexadecimal((lsAddr >> 3) << 3)}\n")
              printf(p"Shifted output: ${Hexadecimal(shifted)}\n")
              */
              // Commit
              lsNextState := lsIDLE
            }
          }
        }
      }

      is(Decoder.Op("STORE").ident) {
        switch(lsState) {
          is(lsIDLE) {
            val lsAddrCompute = pipe.rs1val + pipe.instr.instr.imm.asUInt
            val lsAddrShift = lsAddrCompute(2, 0) // By 8 bits
            lsAddr := lsAddrCompute
            d$.addr := (lsAddrCompute >> 3) << 3 // Align
            d$.write := true.B

            val tailMask = Wire(UInt((XLEN/8).W))
            tailMask := 0.U

            /*
            printf("Store emit: \n")
            printf(p"  Addr: ${Hexadecimal(d$.addr)}\n")
            printf(p"  Wdata: ${Hexadecimal(d$.wdata)}\n")
            printf(p"  BE: ${Hexadecimal(d$.be)}\n")
            */
            d$.wdata := pipe.rs2val << (lsAddrShift * 8.U)
            d$.be := tailMask << lsAddrShift

            switch(pipe.instr.instr.funct3) {
              is(Decoder.STORE_FUNC("SB")) { tailMask := 0x01.U }
              is(Decoder.STORE_FUNC("SH")) { tailMask := 0x03.U }
              is(Decoder.STORE_FUNC("SW")) { tailMask := 0x0f.U }
              is(Decoder.STORE_FUNC("SD")) { tailMask := 0xff.U }
            }

            // printf("Transfered by STORE")
            lsNextState := lsWAIT
          }

          is(lsWAIT) {
            when(!d$.stall) {
              lsNextState := lsIDLE
            }
          }
        }
      }
    }

    (ext, lsNextState =/= lsIDLE)
  }
  override def finalize(pipe: PipeInstr, ext: LSUExt): RetireInfo = {
    val info = Wire(new RetireInfo(ADDR_WIDTH, XLEN))
    info.branch.nofire()

    when(pipe.instr.instr.op === Decoder.Op("LOAD").ident) {
      info.regWaddr := pipe.instr.instr.rd
      info.regWdata := ext.wdata
    }.otherwise {
      info.regWaddr := 0.U
      info.regWdata := DontCare
    }

    info
  }
}
