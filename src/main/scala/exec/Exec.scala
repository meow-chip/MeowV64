package exec
import chisel3._
import reg._
import data._
import instr._
import chisel3.util._
import _root_.core.StageCtrl
import cache.DCachePort
import cache.DCache

class BranchResult(val ADDR_WIDTH: Int = 48) extends Bundle {
  val branch = Bool()
  val target = UInt(ADDR_WIDTH.W)
}


class Exec(ADDR_WIDTH: Int, XLEN: Int) extends Module {
  val io = IO(new Bundle {
    val regReaders = Vec(2, new RegReader)
    val regWriter = new RegWriter
    val instr = Input(new InstrExt(ADDR_WIDTH))

    val axi = new AXI(8)

    val ctrl = StageCtrl.stage()

    val branch = Output(new BranchResult(ADDR_WIDTH))
  })

  io.branch := 0.U.asTypeOf(io.branch)
  io.regWriter.addr := 0.U
  io.regWriter.data := 0.U

  val dcache = Module(new DCache(ADDR_WIDTH, XLEN))
  dcache.io <> DontCare
  dcache.io.axi <> io.axi
  dcache.io.pause := false.B

  dcache.io.read := false.B
  dcache.io.write := false.B

  val default = Wire(new InstrExt)
  default.addr := DontCare
  default.instr := 0.U.asTypeOf(default.instr)
  default.vacant := true.B
  val current = RegInit(default)
  val readRs1 = Reg(UInt(XLEN.W))
  val readRs2 = Reg(UInt(XLEN.W))

  printf(p"EX:\n================\n")
  printf(p"Running:\n${current}\n")
  printf(p"readRs1: 0x${Hexadecimal(readRs1)}\n")
  printf(p"readRs2: 0x${Hexadecimal(readRs2)}\n")
  printf(p"Writing To: 0x${Hexadecimal(io.regWriter.addr)}\n")
  printf(p"Writing Data: 0x${Hexadecimal(io.regWriter.data)}\n")

  io.regReaders(0).addr := io.instr.instr.rs1
  io.regReaders(1).addr := io.instr.instr.rs2

  when(!io.ctrl.pause && !io.ctrl.stall) {
    when(!io.ctrl.flush) {
      current := io.instr

      readRs1 := io.regReaders(0).data
      readRs2 := io.regReaders(1).data
    }.otherwise {
      current := default
    }
  }

  // Load/Store related FSM
  val lsIDLE :: lsWAIT :: nil = Enum(2)
  val lsState = RegInit(lsIDLE)
  val lsNextState = Wire(lsState.cloneType)
  val lsAddr = Reg(UInt(ADDR_WIDTH.W))

  lsNextState := lsState
  when(lsNextState === lsWAIT || !io.ctrl.pause) {
    lsState := lsNextState
  }

  printf(p">>>>>>> lsNextState: ${lsNextState}\n")
  io.ctrl.stall := (!current.vacant) && lsNextState =/= lsIDLE

  when(!current.vacant) {
    switch(current.instr.op) {

      // Arith/Logical

      is(Decoder.Op("OP-IMM").ident) {
        when(!io.ctrl.pause) {
          io.regWriter.addr := current.instr.rd
        }
        val extended = Wire(SInt(64.W))
        extended := current.instr.imm

        switch(current.instr.funct3) {
          is(Decoder.OP_FUNC("ADD/SUB")) { // Can only be ADDI in OP-IMM
            io.regWriter.data := (extended + readRs1.asSInt).asUInt
          }
          is(Decoder.OP_FUNC("SLL")) {
            io.regWriter.data := (readRs1 << extended(4, 0)).asUInt
          }

          is(Decoder.OP_FUNC("SLT")) {
            when(readRs1.asSInt < extended) {
              io.regWriter.data := 1.U
            }.otherwise {
              io.regWriter.data := 0.U
            }
          }

          is(Decoder.OP_FUNC("SLTU")) {
            when(readRs1 < extended.asUInt) {
              io.regWriter.data := 1.U
            }.otherwise {
              io.regWriter.data := 0.U
            }
          }

          is(Decoder.OP_FUNC("XOR")) {
            io.regWriter.data := extended.asUInt ^ readRs1
          }

          is(Decoder.OP_FUNC("SRL/SRA")) {
            when(current.instr.funct7(5)) {
              // SRA
              io.regWriter.data := (readRs1.asSInt >> extended(4, 0)).asUInt
            }.otherwise {
              // SRL
              io.regWriter.data := readRs1 >> extended(4, 0).asUInt
            }
          }

          is(Decoder.OP_FUNC("OR")) {
            io.regWriter.data := extended.asUInt | readRs1
          }

          is(Decoder.OP_FUNC("AND")) {
            io.regWriter.data := extended.asUInt & readRs1
          }
        }
      }

      is(Decoder.Op("OP-IMM-32").ident) {
        when(!io.ctrl.pause) {
          io.regWriter.addr := current.instr.rd
        }

        // First we truncate everything to 32-bit, get a 32-bit result, then
        // sign-extend to 64-bit. TODO: Not sure if it works
        val result32 = Wire(SInt(32.W))
        result32 := 0.S // Default value

        switch(current.instr.funct3) {
          is(Decoder.OP_FUNC("ADD/SUB")) {
            // ADDIW
            result32 := readRs1.asSInt + current.instr.imm
          }

          is(Decoder.OP_FUNC("SLL")) {
            // SLLIW
            // TODO: add assert to check shamt[5]
            result32 := readRs1.asSInt << current.instr.imm(4, 0)
          }

          is(Decoder.OP_FUNC("SRL/SRA")) {
            when(current.instr.funct7(5)) {
              // SRAIW
              result32 := readRs1(31, 0).asSInt >> current.instr.imm(4, 0)
            }.otherwise {
              // SRLIW
              result32 := (readRs1(31, 0).asUInt >> current.instr.imm(4, 0)).asSInt
            }
          }
        }

        val result = Wire(SInt(64.W))
        result := result32
        io.regWriter.data := result.asUInt
      }

      is(Decoder.Op("OP").ident) {
        when(!io.ctrl.pause) {
          io.regWriter.addr := current.instr.rd
        }

        switch(current.instr.funct3) {
          is(Decoder.OP_FUNC("ADD/SUB")) {
            when(current.instr.funct7(5)) {
              // Overflows ignored in ADD/SUB
              // SUB
              io.regWriter.data := readRs1 - readRs2
            }.otherwise {
              // ADD
              io.regWriter.data := readRs1 + readRs2
            }
          }

          is(Decoder.OP_FUNC("SLL")) {
            io.regWriter.data := readRs1 << readRs2(5, 0)
          }

          is(Decoder.OP_FUNC("SLT")) {
            when(readRs1.asSInt < readRs2.asSInt) {
              io.regWriter.data := 1.U
            }.otherwise {
              io.regWriter.data := 0.U
            }
          }

          is(Decoder.OP_FUNC("SLTU")) {
            when(readRs1.asUInt < readRs2.asUInt) {
              io.regWriter.data := 1.U
            }.otherwise {
              io.regWriter.data := 0.U
            }
          }

          is(Decoder.OP_FUNC("XOR")) {
            io.regWriter.data := readRs1 ^ readRs2
          }

          is(Decoder.OP_FUNC("SRL/SRA")) {
            when(current.instr.funct7(5)) {
              // SRA
              // In RV64I, only the low 6 bits of rs2 are considered for the
              // shift amount. (c.f. spec p.53)
              io.regWriter.data := (readRs1.asSInt >> readRs2(5, 0)).asUInt
            }.otherwise {
              io.regWriter.data := readRs1 >> readRs2(5, 0)
            }
          }

          is(Decoder.OP_FUNC("OR")) {
            io.regWriter.data := readRs1 | readRs2
          }

          is(Decoder.OP_FUNC("AND")) {
            io.regWriter.data := readRs1 & readRs2
          }
        }
      }

      is(Decoder.Op("OP-32").ident) {
        when(!io.ctrl.pause) {
          io.regWriter.addr := current.instr.rd
        }
        val result32 = Wire(UInt(32.W))
        result32 := 0.U // Default value

        switch(current.instr.funct3) {
          is(Decoder.OP_FUNC("ADD/SUB")) {
            when(current.instr.funct7(5)) {
              // SUBW
              result32 := readRs1 - readRs2
            }.otherwise {
              // ADDW
              result32 := readRs1 + readRs2
            }
          }

          is(Decoder.OP_FUNC("SLL")) {
            // SLLW
            result32 := readRs1 << readRs2(4, 0)
          }

          is(Decoder.OP_FUNC("SRL/SRA")) {
            when(current.instr.funct7(5)) {
              // SRAW
              result32 := (readRs1(31, 0).asSInt >> readRs2(4, 0)).asUInt
            }.otherwise {
              // SRLW
              result32 := readRs1(31, 0).asUInt >> readRs2(4, 0)
            }
          }
        }

        // Sign extended
        val result = Wire(SInt(64.W))
        result := result32.asSInt
        io.regWriter.data := result.asUInt
      }

      // Load immediate

      is(Decoder.Op("LUI").ident) {
        io.regWriter.addr := current.instr.rd
        val extended = Wire(SInt(64.W))
        extended := current.instr.imm
        io.regWriter.data := extended.asUInt
      }

      is(Decoder.Op("AUIPC").ident) {
        io.regWriter.addr := current.instr.rd
        val result = Wire(SInt(64.W))
        result := current.instr.imm + current.addr.asSInt
        io.regWriter.data := result.asUInt
      }

      // Load/Store

      is(Decoder.Op("LOAD").ident) {
        switch(lsState) {
          is(lsIDLE) {
            val lsAddrCompute = readRs1 + current.instr.imm.asUInt
            lsAddr := lsAddrCompute
            dcache.io.addr := (lsAddrCompute >> 3) << 3 // Align
            dcache.io.read := true.B

            lsNextState := lsWAIT
          }

          is(lsWAIT) {
            val data = dcache.io.rdata
            val shifted = data >> (lsAddr(2, 0) * 8.U)
            val result = Wire(UInt(XLEN.W))
            val signedResult = Wire(SInt(XLEN.W))

            signedResult := DontCare
            result := signedResult.asUInt
            io.regWriter.data := result

            switch(current.instr.funct3) {
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

            when(!dcache.io.stall) {
              printf(p"Load recv:\n  Rdata: ${Hexadecimal(dcache.io.rdata)}\n")
              printf(p"Addr: ${Hexadecimal((lsAddr >> 3) << 3)}\n")
              printf(p"Shifted output: ${Hexadecimal(shifted)}\n")
              // Commit
              io.regWriter.addr := current.instr.rd
              lsNextState := lsIDLE
            }
          }
        }
      }

      is(Decoder.Op("STORE").ident) {
        switch(lsState) {
          is(lsIDLE) {
            val lsAddrCompute = readRs1 + current.instr.imm.asUInt
            val lsAddrShift = lsAddrCompute(2, 0) // By 8 bits
            lsAddr := lsAddrCompute
            dcache.io.addr := (lsAddrCompute >> 3) << 3 // Align
            dcache.io.write := true.B

            val tailMask = Wire(UInt((XLEN/8).W))
            tailMask := 0.U

            printf("Store emit: \n")
            printf(p"  Addr: ${Hexadecimal(dcache.io.addr)}\n")
            printf(p"  Wdata: ${Hexadecimal(dcache.io.wdata)}\n")
            printf(p"  BE: ${Hexadecimal(dcache.io.be)}\n")
            dcache.io.wdata := readRs2 << (lsAddrShift * 8.U)
            dcache.io.be := tailMask << lsAddrShift

            switch(current.instr.funct3) {
              is(Decoder.STORE_FUNC("SB")) { tailMask := 0x01.U }
              is(Decoder.STORE_FUNC("SH")) { tailMask := 0x03.U }
              is(Decoder.STORE_FUNC("SW")) { tailMask := 0x0f.U }
              is(Decoder.STORE_FUNC("SD")) { tailMask := 0xff.U }
            }

            lsNextState := lsWAIT
          }

          is(lsWAIT) {
            when(!dcache.io.stall) {
              lsNextState := lsIDLE
            }
          }
        }
      }

      // Branch

      is(Decoder.Op("JAL").ident) {
        val linked = current.addr + 4.U
        val dest = current.instr.imm + current.addr.asSInt
        io.branch.branch := true.B
        io.branch.target := dest.asUInt

        io.regWriter.addr := current.instr.rd
        io.regWriter.data := linked.asUInt
      }

      is(Decoder.Op("JALR").ident) {
        val linked = current.addr + 4.U
        val dest = readRs1.asSInt + current.addr.asSInt
        io.branch.branch := true.B
        io.branch.target := dest.asUInt

        io.regWriter.addr := current.instr.rd
        io.regWriter.data := linked.asUInt
      }

      is(Decoder.Op("BRANCH").ident) {
        val branch = Wire(Bool())
        branch := false.B

        val readRs1S = readRs1.asSInt
        val readRs2S = readRs2.asSInt
        switch(current.instr.funct3) {
          is(Decoder.BRANCH_FUNC("BEQ")) {
            branch := readRs1 === readRs2
          }

          is(Decoder.BRANCH_FUNC("BNE")) {
            branch := readRs1 =/= readRs2
          }

          is(Decoder.BRANCH_FUNC("BLT")) {
            branch := readRs1S < readRs2S
          }

          is(Decoder.BRANCH_FUNC("BGE")) {
            branch := readRs1S > readRs2S
          }

          is(Decoder.BRANCH_FUNC("BLTU")) {
            branch := readRs1 < readRs2
          }

          is(Decoder.BRANCH_FUNC("BGEU")) {
            branch := readRs1 > readRs2
          }
        }

        io.branch.branch := branch
        io.branch.target := (current.instr.imm + current.addr.asSInt).asUInt
      }
    }
  }.otherwise {
    printf("Vacant, skipped exec")
  }
}
