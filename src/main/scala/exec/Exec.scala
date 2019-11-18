package exec
import chisel3._
import reg._
import data._
import instr._
import chisel3.util._
import _root_.core.StageCtrl
import cache.DCachePort
import cache.DCache
import instr.Decoder.InstrType

class BranchResult(val ADDR_WIDTH: Int = 48) extends Bundle {
  val branch = Bool()
  val target = UInt(ADDR_WIDTH.W)

  def nofire() = {
    branch := false.B
    target := DontCare
  }

  def fire(addr: UInt) = {
    branch := true.B
    target := addr
  }
}


class Exec(ADDR_WIDTH: Int, XLEN: Int, ISSUE_NUM: Int) extends Module {
  val io = IO(new Bundle {
    val regReaders = Vec(2, new RegReader)
    val regWriter = new RegWriter
    val instr = Input(Vec(ISSUE_NUM, new InstrExt(ADDR_WIDTH)))

    val axi = new AXI(XLEN)

    val ctrl = StageCtrl.stage()

    val branch = Output(new BranchResult(ADDR_WIDTH))
  })

  io.branch := 0.U.asTypeOf(io.branch)
  io.regWriter.addr := 0.U
  io.regWriter.data := 0.U

  val dcache = Module(new DCache(ADDR_WIDTH, XLEN))

  val default = Wire(Vec(ISSUE_NUM, new InstrExt))
  for(i <- default) {
    i.addr := DontCare
    i.instr := 0.U.asTypeOf(i.instr)
    i.vacant := true.B
  }
  val current = RegInit(default)
  val instr = RegInit(0.U(log2Ceil(ISSUE_NUM+1).W))
  val readRs1 = Wire(UInt(XLEN.W))
  val readRs2 = Wire(UInt(XLEN.W))

  val branched = RegInit(false.B)
  val branchedAddr = Reg(UInt(ADDR_WIDTH.W))

  io.branch.branch := branched
  io.branch.target := branchedAddr

  io.regReaders(0).addr := current(instr).instr.rs1
  io.regReaders(1).addr := current(instr).instr.rs2
  readRs1 := io.regReaders(0).data
  readRs2 := io.regReaders(1).data

  val alu = Module(new ALU(ADDR_WIDTH, XLEN))
  val imm = Module(new Imm(ADDR_WIDTH, XLEN))
  val lsu = Module(new LSU(ADDR_WIDTH, XLEN))
  val br = Module(new Branch(ADDR_WIDTH, XLEN))
  val mul = Module(new Mul(ADDR_WIDTH, XLEN))
  val div = Module(new Div(ADDR_WIDTH, XLEN, false, 32))
  val div32 = Module(new Div(ADDR_WIDTH, XLEN, true, 16))

  lsu.d$ <> dcache.io
  lsu.axi <> io.axi

  val units = List(alu, imm, lsu, br, mul, div, div32)

  val placeholder = Wire(new PipeInstr(ADDR_WIDTH, XLEN))
  placeholder := 0.U.asTypeOf(placeholder)
  placeholder.instr.vacant := true.B

  var stall = false.B
  for(u <- units) {
    stall = stall || u.io.stall
  }

  val sIDLE :: sRUNNING :: nil = Enum(2)
  val unitState = RegInit(sIDLE)
  val unitStateNext = Wire(sIDLE.cloneType)
  unitStateNext := unitState

  when(!stall) {
    unitState := unitStateNext
  }

  val substall = (!branched) && (!current(instr).vacant) && (stall || unitStateNext =/= sIDLE)
  io.ctrl.stall := instr =/= ISSUE_NUM.U && !branched

  when(!substall && instr === (ISSUE_NUM-1).U) {
    io.ctrl.stall := false.B
  }

  when(!io.ctrl.pause && !io.ctrl.stall) {
    branched := false.B
    when(!io.ctrl.flush) {
      /*
      printf("[FETCH]: \n")
      printf(p"${io.instr}")
      printf("\n[FETCH]")
      */
      current := io.instr
    }.otherwise {
      current := default
    }
  }

  /*
  when(!substall && instr < ISSUE_NUM.U) {
    printf(p"EX:\n================\n")
    printf(p"Running: ${Hexadecimal(current(instr).addr)}\n")
    printf(p"readRs1: 0x${Hexadecimal(readRs1)}\n")
    printf(p"readRs2: 0x${Hexadecimal(readRs2)}\n")
    printf(p"Writing To: 0x${Hexadecimal(io.regWriter.addr)}\n")
    printf(p"Writing Data: 0x${Hexadecimal(io.regWriter.data)}\n")
  }
  */

  when(!io.ctrl.stall && !io.ctrl.pause) {
    instr := 0.U
  }.elsewhen(!substall && instr =/= ISSUE_NUM.U) {
    instr := instr + 1.U
  }

  val unitInput = Wire(new PipeInstr(ADDR_WIDTH, XLEN))
  unitInput.instr := current(instr)
  unitInput.rs1val := readRs1
  unitInput.rs2val := readRs2

  /*
  printf(p">>>>>>>>>>>>>>>\n")
  printf(p"nextup: ${unitInput}\n")
  */

  for(u <- units) {
    u.io.next := placeholder
  }

  when(!branched && instr =/= ISSUE_NUM.U && !current(instr).vacant && unitState === sIDLE) {
    unitStateNext := sRUNNING

    // printf(p"current instr: ${instr}\n")
    switch(current(instr).instr.op) {

      // Arith/Logical
      is(Decoder.Op("OP").ident,
        Decoder.Op("OP-IMM").ident) {
          when(
            current(instr).instr.op === Decoder.Op("OP").ident
            && current(instr).instr.funct7 === Decoder.MULDIV_FUNCT7
          ) {
            when(current(instr).instr.funct3(2)) { // funct3 >= 0b100 ==> DIV/REM
              div.io.next := unitInput
            }.otherwise {
              mul.io.next := unitInput
            }
          }.otherwise {
            alu.io.next := unitInput
          }
        }

      is(Decoder.Op("OP-32").ident,
        Decoder.Op("OP-IMM-32").ident) {
          when(
            current(instr).instr.op === Decoder.Op("OP-32").ident
            && current(instr).instr.funct7 === Decoder.MULDIV_FUNCT7
          ) {
            when(current(instr).instr.funct3(2)) { // funct3 >= 0b100 ==> DIV/REM
              div32.io.next := unitInput
            }.otherwise {
              mul.io.next := unitInput
            }
          }.otherwise {
            alu.io.next := unitInput
          }
        }

      // Load immediate
      is(Decoder.Op("AUIPC").ident,
        Decoder.Op("LUI").ident) {
          imm.io.next := unitInput
        }

      // Load/Store
      is(Decoder.Op("LOAD").ident,
        Decoder.Op("STORE").ident) {
          lsu.io.next := unitInput
        }

      // Branch
      is(Decoder.Op("JAL").ident,
        Decoder.Op("JALR").ident,
        Decoder.Op("BRANCH").ident) {
          br.io.next := unitInput
        }

    }
  }.otherwise {
    // printf("Vacant, skipped exec")
  }

  for(u <- units) {
    u.io.pause := false.B

    /*
    when(u.io.stall) {
      printf(p"[STALL ]: Stalled by ${u.name}\n")
    }
    */

    when(u.io.retirement.branch.branch) {
      branched := true.B
      branchedAddr := u.io.retirement.branch.target
      io.branch := u.io.retirement.branch
      // printf(p"[BRANCH] ${Hexadecimal(branchedAddr)}\n")
    }

    when(u.io.retirement.regWaddr =/= 0.U) {
      io.regWriter.addr := u.io.retirement.regWaddr
      io.regWriter.data := u.io.retirement.regWdata
      // printf(p"[COMMIT]: By ${u.name}\n")
    }

    when(!u.io.retired.instr.vacant && !u.io.stall) {
      unitStateNext := sIDLE
    }
  }

  /*
  when(io.regWriter.addr =/= 0.U) {
    when(io.regWriter.addr =/= RegNext(io.regWriter.addr) || io.regWriter.data =/= RegNext(io.regWriter.data)) {
      printf(p"[COMMIT]: ${Decimal(io.regWriter.addr)} <- ${Hexadecimal(io.regWriter.data)}\n")
    }
  }
  */
}
