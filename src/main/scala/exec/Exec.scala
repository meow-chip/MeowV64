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

  val alu = Module(new ALU(ADDR_WIDTH, XLEN, false))
  val alu32 = Module(new ALU(ADDR_WIDTH, XLEN, true))
  val imm = Module(new Imm(ADDR_WIDTH, XLEN))
  val lsu = Module(new LSU(ADDR_WIDTH, XLEN))
  val br = Module(new Branch(ADDR_WIDTH, XLEN))

  lsu.d$ <> dcache.io
  lsu.axi <> io.axi

  val units = List(alu, alu32, imm, lsu, br)

  val placeholder = Wire(new PipeInstr(ADDR_WIDTH, XLEN))
  placeholder := 0.U.asTypeOf(placeholder)
  placeholder.instr.vacant := true.B

  var stall = false.B

  val sIDLE :: sRUNNING :: nil = Enum(2)
  val unitState = RegInit(sIDLE)
  val unitStateNext = Wire(sIDLE.cloneType)
  unitState := unitStateNext
  unitStateNext := unitState

  for(u <- units) {
    u.io.next := placeholder
    u.io.pause := io.ctrl.pause

    stall = stall || u.io.stall

    when(u.io.retirement.branch.branch) {
      branched := true.B
      branchedAddr := u.io.retirement.branch.target
      io.branch := u.io.retirement.branch
    }

    when(u.io.retirement.regWaddr =/= 0.U) {
      io.regWriter.addr := u.io.retirement.regWaddr
      io.regWriter.data := u.io.retirement.regWdata

      unitStateNext := sIDLE
    }
  }

  val substall = (!branched) && (!current(instr).vacant) && stall || unitStateNext =/= sIDLE
  io.ctrl.stall := instr =/= ISSUE_NUM.U || substall

  when(!io.ctrl.pause && !io.ctrl.stall) {
    branched := false.B
    when(!io.ctrl.flush) {
      current := io.instr
    }.otherwise {
      current := default
    }
  }

  /*
  when(!substall && instr < ISSUE_NUM.U) {
    printf(p"EX:\n================\n")
    printf(p"Running:\n${current(instr)}\n")
    printf(p"readRs1: 0x${Hexadecimal(readRs1)}\n")
    printf(p"readRs2: 0x${Hexadecimal(readRs2)}\n")
    printf(p"Writing To: 0x${Hexadecimal(io.regWriter.addr)}\n")
    printf(p"Writing Data: 0x${Hexadecimal(io.regWriter.data)}\n")
  }
  */

  when(!substall) {
    when(instr =/= ISSUE_NUM.U) {
      instr := instr + 1.U

      when(instr === (ISSUE_NUM-1).U) {
        io.ctrl.stall := false.B
        when(!io.ctrl.pause) {
          instr := 0.U
        }
      }
    }.elsewhen(!io.ctrl.pause) {
      instr := 0.U
    }
  }

  val unitInput = Wire(new PipeInstr(ADDR_WIDTH, XLEN))
  unitInput.instr := current(instr)
  unitInput.rs1val := readRs1
  unitInput.rs2val := readRs2

  when(!branched && instr =/= ISSUE_NUM.U && !current(instr).vacant && unitState === sIDLE) {
    unitStateNext := sRUNNING

    // printf(p"current instr: ${instr}\n")
    switch(current(instr).instr.op) {

      // Arith/Logical
      is(Decoder.Op("OP").ident,
        Decoder.Op("OP-IMM").ident) {
          alu.io.next := unitInput
        }

      is(Decoder.Op("OP-32").ident,
        Decoder.Op("OP-IMM-32").ident) {
          alu32.io.next := unitInput
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
}
