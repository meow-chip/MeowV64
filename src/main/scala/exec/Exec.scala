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

/**
 * Out-of-order exection (Tomasulo's algorithm)
 * 
 * First we check if instructions are eligible to be issues. Criterias include:
 * - Target reservation station has free slots
 * - Number of in-flight instructions haven't exceeded the limit.
 *   This limit affects our rob buffer length, as well as renamed reg tags' length
 * - Issue FIFO is not depleted
 */
class Exec(implicit val coredef: CoreDef) extends MultiIOModule {
  val io = IO(new Bundle {
    val ctrl = StageCtrl.stage()

    val branch = Output(new BranchResult)
    val brSrc = Output(UInt(coredef.ADDR_WIDTH.W))

    val csrWriter = new CSRWriter(coredef.XLEN)
  })

  // We don't stall now
  io.ctrl.stall := false.B

  val rr = IO(Vec(coredef.ISSUE_NUM*2, new RegReader))
  val rw = IO(Vec(coredef.RETIRE_NUM, new RegWriter))

  val toIF = IO(new InstrFifoReader(coredef))

  val toDC = IO(new Bundle {
    val r = new DCReader(coredef.L1D)
    val w = new DCWriter(coredef.L1D)
  })

  val cdb = Wire(new CDB)

  val renamer = Module(new Renamer)
  renamer.rr <> rr
  renamer.cdb := cdb
  renamer.toExec.flush := io.ctrl.flush

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
      }
    )),
    Module(new UnitSel(
      Seq(
        Module(new ALU).suggestName("ALU"),
        Module(new Mul).suggestName("Mul"),
        Module(new Div(4)).suggestName("Div")
      ),
      instr => {
        val isMul = (
          instr.funct7 === Decoder.MULDIV_FUNCT7 && (
            instr.funct3 === Decoder.MULDIV_FUNC("MUL") ||
            instr.funct3 === Decoder.MULDIV_FUNC("MULH") ||
            instr.funct3 === Decoder.MULDIV_FUNC("MULHSU") ||
            instr.funct3 === Decoder.MULDIV_FUNC("MULHU")
          )
        )

        val isDiv = instr.funct7 === Decoder.MULDIV_FUNCT7 && !isMul

        Seq(!isMul && !isDiv, isMul, isDiv)
      }
    )),
    Module(new UnitSel(
      Seq(
        Module(new LSU).suggestName("LSU")
      ),
      instr => Seq(true.B)
    ))

  )

  // Connect extra ports
  units(0).extras("CSR") <> io.csrWriter
  units(2).extras("LSU") <> toDC.r

  assume(units.length == coredef.UNIT_COUNT)
  // TODO: asserts Bypass is in unit 0

  val stations = units.zipWithIndex.map({ case (u, idx)=> {
    val rs = Module(new ResStation(idx)).suggestName(s"ResStation_${idx}")
    rs.cdb := cdb
    rs.exgress <> u.rs

    rs
  }})

  for(s <- stations) {
    s.cdb := cdb
    s.ctrl.flush := io.ctrl.flush

    // By default: nothing pushes
    s.ingress := DontCare
    s.ingress.push := false.B
  }

  for(u <- units) {
    u.ctrl.flush := io.ctrl.flush
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

  // Issue
  val maxIssueNum = retirePtr -% issuePtr -% 1.U // issuePtr cannot reach retirePtr
  assert(issueNum <= maxIssueNum)

  val canIssue = Wire(Vec(coredef.ISSUE_NUM, Bool()))
  var taken = 0.U(coredef.UNIT_COUNT.W)
  issueNum := 0.U

  for(idx <- (0 until coredef.ISSUE_NUM)) {
    val selfCanIssue = Wire(Bool()).suggestName(s"selfCanIssue_$idx")
    val sending = Wire(UInt(coredef.UNIT_COUNT.W)).suggestName(s"sending_$idx")
    val instr = Wire(new ReservedInstr)

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

      when(!applicables.orR()) {
        // Is an invalid instruction
        selfCanIssue := stations(0).ingress.free && !taken(0)
        sending := 1.U
        renamer.toExec.invalMap(idx) := true.B
      }.otherwise {
        val mask = applicables & avails & ~taken
        mask.suggestName(s"mask_$idx")

        // Find lowest set
        sending := MuxCase(0.U, (0 until coredef.UNIT_COUNT).map(idx => (
          mask(idx), (1<<idx).U
        )))

        selfCanIssue := mask.orR()
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
    }

    taken = taken | sending
  }

  // Filling ROB & CDB broadcast
  for((u, ent) <- units.zip(cdb.entries)) {
    ent.valid := false.B
    ent.name := 0.U // Helps to debug, because we are asserting store(0) === 0
    ent.data := u.retire.info.wb

    when(!u.ctrl.stall && !u.retire.instr.instr.vacant) {
      ent.name := u.retire.instr.rdname
      ent.valid := ent.name =/= 0.U

      rob(u.retire.instr.tag).retirement := u.retire
      rob(u.retire.instr.tag).valid := true.B
    }
  }

  // Commit
  // FIXME: Uncached
  // FIXME: make CSR act as FENCE

  retireNum := 0.U

  val canRetire = Wire(Vec(coredef.RETIRE_NUM, Bool()))
  val isBranch = Wire(Vec(coredef.RETIRE_NUM, Bool()))

  // Default: no branch if nothing is retired
  io.branch.nofire()
  io.brSrc := DontCare

  val memAccSucc = Wire(Bool())
  memAccSucc := false.B

  // Compute if we can retire a certain instruction
  for(idx <- (0 until coredef.RETIRE_NUM)) {
    val info = rob(retirePtr +% idx.U)
    isBranch(idx) := info.retirement.info.branch.branched()

    if(idx == 0) {
      // Allow retirement only after mem access is finished
      canRetire(idx) := info.valid && (memAccSucc || info.retirement.info.mem.isNoop())
    } else {
      // Only allow mem ops in the first retire slot
      canRetire(idx) := info.valid && canRetire(idx - 1) && !isBranch(idx-1) && info.retirement.info.mem.isNoop()
    }

    when(canRetire(idx)) {
      rw(idx).addr := info.retirement.instr.instr.instr.getRd
      rw(idx).data := info.retirement.info.wb
      // Branching. canRetire(idx) -> \forall i < idx, !isBranch(i)
      // So we can safely sets io.branch to the last valid branch info
      io.branch := info.retirement.info.branch
      io.brSrc := info.retirement.instr.instr.addr

      info.valid := false.B

      retireNum := (1+idx).U
    }.otherwise {
      rw(idx).addr := 0.U
      rw(idx).data := DontCare
    }
  }

  // Mem opts
  toDC.w := DontCare
  toDC.w.write := false.B

  val retireNext = rob(retirePtr)
  when(retireNext.valid) {
    val memAcc = retireNext.retirement.info.mem
    switch(memAcc.op) {
      is(MemSeqAccOp.s) {
        toDC.w.addr := memAcc.addr
        toDC.w.be := memAcc.be
        toDC.w.data := retireNext.retirement.info.wb
        toDC.w.write := true.B

        memAccSucc := !toDC.w.stall
      }
    }
  }

  // Asserts that at most one can branch
  val branchMask = isBranch.asUInt() & canRetire.asUInt()
  assert(!(branchMask & (branchMask-%1.U)).orR)

  // Asserts that canRetire forms a tailing 1 sequence, e.g.: 00011111
  assert(!(canRetire.asUInt & (canRetire.asUInt+%1.U)).orR)

  // Flushing control
  when(io.ctrl.flush) {
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
        Decoder.Op("STORE").ident
      ) {
        ret := "b100".U(coredef.UNIT_COUNT.W)
      }
    }

    ret
  }
}
