package exec
import chisel3._
import reg._
import data._
import instr._
import chisel3.util._
import _root_.core.StageCtrl
import instr.Decoder.InstrType
import _root_.core.CSRWriter
import _root_.core.CoreDef
import cache.DCReader
import cache.DCWriter
import exec.units._
import exec.UnitSel.Retirement
import _root_.core.Core
import exec.Exec.ROBEntry
import _root_.core.CSROp
import cache.L1UCPort
import _root_.core.ExReq
import Chisel.experimental.chiselName
import cache.DCFenceStatus

/**
 * Out-of-order exection (Tomasulo's algorithm)
 * 
 * First we check if instructions are eligible to be issues. Criterias include:
 * - Target reservation station has free slots
 * - Number of in-flight instructions haven't exceeded the limit.
 *   This limit affects our rob buffer length, as well as renamed reg tags' length
 * - Issue FIFO is not depleted
 */
@chiselName
class Exec(implicit val coredef: CoreDef) extends MultiIOModule {
  val toCtrl = IO(new Bundle {
    val ctrl = StageCtrl.stage()
    val retCnt = Output(UInt(log2Ceil(coredef.RETIRE_NUM+1).W))
    val nepc = Output(UInt(coredef.XLEN.W))

    val branch = Output(new BranchResult)
    val tval = Output(UInt(coredef.XLEN.W))

    val int = Input(Bool())
    val intAck = Output(Bool())
  })

  val csrWriter = IO(new CSRWriter(coredef.XLEN))

  // We don't stall now
  toCtrl.ctrl.stall := false.B

  val rr = IO(Vec(coredef.ISSUE_NUM*2, new RegReader))
  val rw = IO(Vec(coredef.RETIRE_NUM, new RegWriter))

  val toIF = IO(new InstrFifoReader(coredef))

  val toDC = IO(new Bundle {
    val r = new DCReader(coredef.L1D)
    val w = new DCWriter(coredef.L1D)
    val fs = new DCFenceStatus(coredef.L1D)
    val u = new L1UCPort(coredef.L1D)
  })

  val cdb = Wire(new CDB)

  val renamer = Module(new Renamer)
  renamer.rr <> rr
  renamer.cdb := cdb
  renamer.toExec.flush := toCtrl.ctrl.flush

  val memAccSucc = Wire(Bool())
  val memAccWB = Wire(Bool())
  val memAccData = Wire(UInt(coredef.XLEN.W))

  // Units
  val units = Seq(
    Module(new UnitSel(
      Seq(
        Module(new ALU).suggestName("ALU"),
        Module(new Branch).suggestName("Branch"),
        Module(new CSR).suggestName("CSR"),
        Module(new Bypass).suggestName("Bypass")
      ),
      instr => {
        val gotoALU = (
          instr.op === Decoder.Op("OP").ident ||
          instr.op === Decoder.Op("OP-IMM").ident ||
          instr.op === Decoder.Op("OP-32").ident ||
          instr.op === Decoder.Op("OP-IMM-32").ident
        )

        val gotoBr = (
          instr.op === Decoder.Op("JALR").ident ||
          instr.op === Decoder.Op("BRANCH").ident ||
          instr.op === Decoder.Op("SYSTEM").ident && instr.funct3 === Decoder.SYSTEM_FUNC("PRIV")
        )

        val gotoCSR = (
          instr.op === Decoder.Op("SYSTEM").ident && instr.funct3 =/= Decoder.SYSTEM_FUNC("PRIV")
        )

        Seq(
          gotoALU,
          gotoBr,
          gotoCSR,
          !gotoALU && !gotoBr && !gotoCSR
        )
      },
      Some(3)
    )),
    Module(new UnitSel(
      Seq(
        Module(new ALU).suggestName("ALU"),
        Module(new Mul).suggestName("Mul"),
        Module(new Div(4)).suggestName("Div")
      ),
      instr => {
        val regALU = (
          instr.op === Decoder.Op("OP").ident ||
          instr.op === Decoder.Op("OP-32").ident
        )
        val isMul = (
          regALU &&
          instr.funct7 === Decoder.MULDIV_FUNCT7 && (
            instr.funct3 === Decoder.MULDIV_FUNC("MUL") ||
            instr.funct3 === Decoder.MULDIV_FUNC("MULH") ||
            instr.funct3 === Decoder.MULDIV_FUNC("MULHSU") ||
            instr.funct3 === Decoder.MULDIV_FUNC("MULHU")
          )
        )

        val isDiv = regALU && instr.funct7 === Decoder.MULDIV_FUNCT7 && !isMul

        Seq(!isMul && !isDiv, isMul, isDiv)
      }
    )),
    Module(new UnitSel(
      Seq(
        Module(new LSU).suggestName("LSU")
      ),
      instr => Seq(true.B),
      hasPipe = false
    ))

  )

  // Connect extra ports
  units(0).extras("CSR") <> csrWriter
  units(2).extras("LSU") <> toDC.r

  assume(units.length == coredef.UNIT_COUNT)
  // TODO: asserts Bypass is in unit 0

  val stations = units.zipWithIndex.map({ case (u, idx)=> {
    val rs = if(idx != 2) {
      Module(new OoOResStation(idx)).suggestName(s"ResStation_${idx}")
    } else {
      val lsb = Module(new LSBuf(idx)).suggestName(s"LSBuf")
      lsb.saUp := units(2).extras("saUp")
      lsb.saDown := memAccSucc
      lsb.fs := toDC.fs
      lsb
    }
    rs.cdb := cdb
    rs.exgress <> u.rs

    rs
  }})

  for(s <- stations) {
    s.cdb := cdb
    s.ctrl.flush := toCtrl.ctrl.flush

    // By default: nothing pushes
    s.ingress := DontCare
    s.ingress.push := false.B
  }

  for(u <- units) {
    u.ctrl.flush := toCtrl.ctrl.flush
  }

  // ROB & ptrs
  val rob = RegInit(VecInit(Seq.fill(coredef.INFLIGHT_INSTR_LIMIT)(ROBEntry.empty)))
  val retirePtr = RegInit(0.U(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W))
  val issuePtr = RegInit(0.U(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W))

  val retireNum = Wire(UInt(log2Ceil(coredef.RETIRE_NUM + 1).W))
  val issueNum = Wire(UInt(log2Ceil(coredef.ISSUE_NUM + 1).W))

  toIF.pop := issueNum
  renamer.toExec.commit := issueNum
  renamer.toExec.input := toIF.view
  renamer.toExec.ntag := issuePtr

  issuePtr := issuePtr + issueNum
  retirePtr := retirePtr + retireNum

  toCtrl.retCnt := retireNum

  // Issue
  val maxIssueNum = retirePtr -% issuePtr -% 1.U // issuePtr cannot reach retirePtr
  assert(issueNum <= maxIssueNum)

  val wasGFence = RegInit(false.B)
  val canIssue = Wire(Vec(coredef.ISSUE_NUM, Bool()))
  var taken = 0.U(coredef.UNIT_COUNT.W)
  issueNum := 0.U
  
  for(idx <- (0 until coredef.ISSUE_NUM)) {
    val selfCanIssue = Wire(Bool()).suggestName(s"selfCanIssue_$idx")
    val sending = Wire(UInt(coredef.UNIT_COUNT.W)).suggestName(s"sending_$idx")
    val instr = Wire(new ReservedInstr)

    // Is global fence? (FENCE.I, CSR)
    val isGFence = (
      instr.instr.instr.op === Decoder.Op("SYSTEM").ident && instr.instr.instr.funct7 =/= Decoder.SYSTEM_FUNC("PRIV")
    )

    // At most only one sending
    assert(!(sending & (sending -% 1.U)).orR)
    assert(!selfCanIssue || sending.orR)

    renamer.toExec.invalMap(idx) := false.B
    instr := renamer.toExec.output(idx)

    when(idx.U >= toIF.cnt || idx.U >= maxIssueNum) {
      selfCanIssue := false.B
      sending := 0.U
    }.otherwise {
      // Route to applicable stations
      val applicables = Exec.route(toIF.view(idx).instr)
      val avails = VecInit(stations.map(_.ingress.free)).asUInt()

      sending := 0.U

      when(
        toIF.view(idx).invalAddr
        || toIF.view(idx).instr.base === InstrType.toInt(InstrType.RESERVED)
        || !applicables.orR()
      ) {
        // Is an invalid instruction
        selfCanIssue := stations(0).ingress.free && !taken(0)
        sending := 1.U
        renamer.toExec.invalMap(idx) := true.B
      }.elsewhen(wasGFence && issuePtr =/= retirePtr) {
        // GFence in-flight
        sending := DontCare
        selfCanIssue := false.B
        // TODO: only apply to first instr to optimize timing?
      }.otherwise {
        val mask = applicables & avails & ~taken
        mask.suggestName(s"mask_$idx")

        // Find lowest set
        sending := MuxCase(0.U, (0 until coredef.UNIT_COUNT).map(idx => (
          mask(idx), (1<<idx).U
        )))

        selfCanIssue := mask.orR()

        if(idx != 0) {
          // Cannot issue GFence that is not on the first slot
          when(isGFence) {
            selfCanIssue := false.B
          }
        } else {
          // Block GFence if there is still in-flight instrs
          when(isGFence && retirePtr =/= issuePtr) {
            selfCanIssue := false.B
          }
        }
      }
    }

    if(idx == 0) canIssue(idx) := selfCanIssue
    else canIssue(idx) := selfCanIssue && canIssue(idx-1)

    when(canIssue(idx)) {
      issueNum := (idx+1).U

      for((s, en) <- stations.zip(sending.asBools)) {
        when(en) {
          s.ingress.push := true.B
          s.ingress.instr := instr
        }
      }

      if(idx == 0) {
        wasGFence := isGFence
      }
    }

    taken = taken | sending
  }

  // Filling ROB & CDB broadcast
  for((u, ent) <- units.zip(cdb.entries)) {
    ent.valid := false.B
    ent.name := 0.U // Helps to debug, because we are asserting store(0) === 0
    ent.data := u.retire.info.wb

    when(!u.ctrl.stall && !u.retire.instr.instr.vacant) {
      when(u.retire.info.mem.isNoop()) {
        ent.name := u.retire.instr.rdname
        ent.valid := ent.name =/= 0.U
      }

      rob(u.retire.instr.tag).retirement := u.retire
      rob(u.retire.instr.tag).valid := true.B
    }
  }

  // Commit

  retireNum := 0.U

  // Default: no branch if nothing is retired
  toCtrl.branch.nofire()
  toCtrl.tval := DontCare
  toCtrl.nepc := DontCare

  memAccSucc := false.B

  cdb.entries(coredef.UNIT_COUNT) := DontCare
  cdb.entries(coredef.UNIT_COUNT).valid := false.B

  when(!rob(retirePtr).valid) {
    // First one invalid, cannot retire anything
    retireNum := 0.U
    toCtrl.branch.nofire()
    for(rwp <- rw) {
      rwp.addr := 0.U
      rwp.data := DontCare
    }
  }.elsewhen(!rob(retirePtr).retirement.info.mem.isNoop()) {
    // Is memory operation, wait for memAccSucc
    toCtrl.branch.nofire()
    for(rwp <- rw) {
      rwp.addr := 0.U
      rwp.data := DontCare
    }
    when(memAccSucc) {
      retireNum := 1.U
      rob(retirePtr).valid := false.B
      retirePtr := retirePtr +% 1.U
      when(memAccWB) {
        cdb.entries(coredef.UNIT_COUNT).name := rob(retirePtr).retirement.instr.rdname
        cdb.entries(coredef.UNIT_COUNT).data := memAccData
        cdb.entries(coredef.UNIT_COUNT).valid := true.B
        rw(0).addr := rob(retirePtr).retirement.instr.instr.instr.getRd
        rw(0).data := memAccData
      }
    }.otherwise {
      retireNum := 0.U
    }
  }.otherwise {
    val canRetire = Wire(Vec(coredef.RETIRE_NUM, Bool()))
    val isBranch = Wire(Vec(coredef.RETIRE_NUM, Bool()))

    // First only not memory operation, possible to do multiple retirement
    // Compute if we can retire a certain instruction
    for(idx <- (0 until coredef.RETIRE_NUM)) {
      val info = rob(retirePtr +% idx.U)
      isBranch(idx) := info.retirement.info.branch.branched()

      if(idx == 0) {
        canRetire(idx) := info.valid
      } else {
        // Only allow mem ops in the first retire slot
        canRetire(idx) := info.valid && canRetire(idx - 1) && !isBranch(idx-1) && info.retirement.info.mem.isNoop()
      }

      when(canRetire(idx)) {
        rw(idx).addr := info.retirement.instr.instr.instr.getRd
        rw(idx).data := info.retirement.info.wb

        // Branching. canRetire(idx) -> \forall i < idx, !isBranch(i)
        // So we can safely sets toCtrl.branch to the last valid branch info
        toCtrl.branch := info.retirement.info.branch

        when(info.retirement.info.branch.ex =/= ExReq.none) {
          // Don't write-back exceptioned instr
          rw(idx).addr := 0.U
        }

        toCtrl.nepc := info.retirement.instr.instr.npc
        when(info.retirement.info.branch.ex === ExReq.ex) {
          toCtrl.tval := info.retirement.info.wb
          toCtrl.nepc := info.retirement.instr.instr.addr
        }

        info.valid := false.B

        retireNum := (1+idx).U

        if(idx == 0) {
        }
      }.otherwise {
        rw(idx).addr := 0.U
        rw(idx).data := DontCare
      }
    }

    // Asserts that at most one can branch
    val branchMask = isBranch.asUInt() & canRetire.asUInt()
    assert(!(branchMask & (branchMask-%1.U)).orR)

    // Asserts that canRetire forms a tailing 1 sequence, e.g.: 00011111
    assert(!(canRetire.asUInt & (canRetire.asUInt+%1.U)).orR)
  }

  // Mem opts
  toDC.w := DontCare
  toDC.w.write := false.B
  toDC.u := DontCare
  toDC.u.read := false.B
  toDC.u.write := false.B

  memAccWB := false.B
  memAccData := DontCare

  val retireNext = rob(retirePtr)
  when(retireNext.valid) {
    val memAcc = retireNext.retirement.info.mem
    switch(memAcc.op) {
      is(MemSeqAccOp.s) {
        toDC.w.addr := memAcc.addr
        toDC.w.be := memAcc.computeBe
        toDC.w.data := retireNext.retirement.info.wb
        toDC.w.write := true.B

        memAccSucc := !toDC.w.stall
      }

      is(MemSeqAccOp.ul) {
        toDC.u.addr := memAcc.addr
        toDC.u.read := true.B

        memAccSucc := !toDC.u.stall
        memAccWB := true.B
        memAccData := memAcc.getSlice(toDC.u.rdata)
      }

      is(MemSeqAccOp.us) {
        toDC.u.addr := memAcc.addr
        toDC.u.write := true.B
        toDC.u.wdata := retireNext.retirement.info.wb
        toDC.u.wstrb := memAcc.computeBe

        memAccSucc := !toDC.u.stall
      }
    }
  }

  // intAck
  when(retireNext.retirement.info.mem.op =/= MemSeqAccOp.no) {
    toCtrl.intAck := false.B
  }.otherwise {
    toCtrl.intAck := toCtrl.int
  }

  // Flushing control
  when(toCtrl.ctrl.flush) {
    issuePtr := 0.U
    retirePtr := 0.U

    for(row <- rob) {
      row.valid := false.B
    }
  }
}

object Exec {
  class ROBEntry(implicit val coredef: CoreDef) extends Bundle {
    val retirement = new Retirement
    val valid = Bool()
  }

  object ROBEntry {
    def empty(implicit coredef: CoreDef) = {
      val ret = Wire(new ROBEntry)
      ret.retirement := DontCare
      ret.valid := false.B

      ret
    }
  }

  def route(instr: Instr)(implicit coredef: CoreDef): UInt = {
    val ret = Wire(UInt(coredef.UNIT_COUNT.W))
    ret := 0.U

    switch(instr.op) {
      is(Decoder.Op("LUI").ident, Decoder.Op("AUIPC").ident) {
        ret := "b001".U(coredef.UNIT_COUNT.W)
      }

      is(Decoder.Op("JALR").ident, Decoder.Op("SYSTEM").ident, Decoder.Op("BRANCH").ident) {
        ret := "b001".U(coredef.UNIT_COUNT.W)
      }

      is(
        Decoder.Op("OP-IMM").ident,
        Decoder.Op("OP-IMM-32").ident,
        Decoder.Op("OP").ident,
        Decoder.Op("OP-32").ident
      ) {
        when(instr.funct7 === Decoder.MULDIV_FUNCT7) {
          ret := "b010".U(coredef.UNIT_COUNT.W)
        }.otherwise {
          ret := "b011".U(coredef.UNIT_COUNT.W)
        }
      }

      is(
        Decoder.Op("LOAD").ident,
        Decoder.Op("STORE").ident,
        Decoder.Op("MEM-MISC").ident
      ) {
        ret := "b100".U(coredef.UNIT_COUNT.W)
      }
    }

    ret
  }
  }
