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
import _root_.util._
import cache.DCReader
import cache.DCWriter
import exec.units._
import exec.UnitSel.Retirement
import _root_.core.Core
import exec.Exec.ROBEntry
import cache.L1UCPort
import _root_.core.ExReq
import Chisel.experimental.chiselName
import cache.DCFenceStatus
import _root_.core.Const
import _root_.core.PrivLevel
import _root_.core.Status
import _root_.core.Satp
import paging.TLBExt

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

    val priv = Input(PrivLevel())
    val status = Input(new Status)

    val tlbrst = Input(Bool())
  })

  val toBPU = IO(new Bundle {
    val valid = Output(Bool())
    val lpc = Output(UInt(coredef.XLEN.W))
    val taken = Output(Bool())
    val hist = Output(new BPUResult)
  })

  val toCore = IO(new Bundle {
    val satp = Input(new Satp)
    val ptw = new TLBExt
  })

  val csrWriter = IO(new CSRWriter(coredef.XLEN))

  // We don't stall now
  toCtrl.ctrl.stall := false.B

  val rr = IO(Vec(coredef.ISSUE_NUM*2, new RegReader))
  val rw = IO(Vec(coredef.RETIRE_NUM, new RegWriter))

  val toIF = IO(new MultiQueueIO(new InstrExt, coredef.ISSUE_NUM))

  val toDC = IO(new Bundle {
    val r = new DCReader
    val w = new DCWriter(coredef.L1D)
    val fs = new DCFenceStatus(coredef.L1D)
    val u = new L1UCPort(coredef.L1D)
  })

  val cdb = Wire(new CDB)

  val renamer = Module(new Renamer)
  for(i <- (0 until coredef.ISSUE_NUM)) {
    renamer.rr(i)(0) <> rr(i * 2)
    renamer.rr(i)(1) <> rr(i * 2 + 1)
  }
  renamer.rw <> rw
  renamer.cdb := cdb
  renamer.toExec.flush := toCtrl.ctrl.flush

  // Inflight instr info
  val inflights = Module(new MultiQueue(new InflightInstr, coredef.INFLIGHT_INSTR_LIMIT, coredef.ISSUE_NUM, coredef.RETIRE_NUM))
  inflights.flush := toCtrl.ctrl.flush

  // Delayed memory ops
  val releaseMem = Wire(DeqIO(new DelayedMemResult))
  releaseMem.nodeq()

  // Units
  val lsu = Module(new LSU).suggestName("LSU");
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
      Some(3),
      hasPipe = false
    )),
    Module(new UnitSel(
      Seq(
        Module(new ALU).suggestName("ALU"),
        Module(new Mul).suggestName("Mul"),
        Module(new Div(16)).suggestName("Div")
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
      },
      hasPipe = false
    )),
    lsu,
  )

  lsu.toMem.reader <> toDC.r
  lsu.toMem.writer <> toDC.w
  lsu.toMem.uncached <> toDC.u
  lsu.release <> releaseMem
  lsu.ptw <> toCore.ptw
  lsu.satp := toCore.satp
  lsu.priv := toCtrl.priv
  lsu.tlbrst := toCtrl.tlbrst
  val hasPendingMem = lsu.hasPending

  // Connect extra ports
  units(0).extras("CSR") <> csrWriter
  units(0).extras("priv") := toCtrl.priv
  units(0).extras("status") := toCtrl.status

  assume(units.length == coredef.UNIT_COUNT)
  // TODO: asserts Bypass is in unit 0

  val stations = units.zipWithIndex.map({ case (u, idx)=> {
    val rs = if(idx != 2) {
      Module(new OoOResStation(idx)).suggestName(s"ResStation_${idx}")
    } else {
      val lsb = Module(new LSBuf(idx)).suggestName(s"LSBuf")
      lsb.hasPending := hasPendingMem
      lsb.fs := toDC.fs
      lsb
    }
    rs.cdb := cdb
    rs.egress <> u.rs

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
    u.flush := toCtrl.ctrl.flush
  }

  // ROB & ptrs
  val rob = RegInit(VecInit(Seq.fill(coredef.INFLIGHT_INSTR_LIMIT)(ROBEntry.empty)))
  val retirePtr = RegInit(0.U(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W))
  val issuePtr = RegInit(0.U(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W))

  val retireNum = Wire(UInt(log2Ceil(coredef.RETIRE_NUM + 1).W))
  val issueNum = Wire(UInt(log2Ceil(coredef.ISSUE_NUM + 1).W))

  toIF.accept := issueNum
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

    instr := renamer.toExec.output(idx)

    when(idx.U >= toIF.cnt || idx.U >= renamer.toExec.count || idx.U >= maxIssueNum) {
      selfCanIssue := false.B
      sending := 0.U
    }.otherwise {
      // Route to applicable stations
      val applicables = Exec.route(toIF.view(idx).instr)
      val avails = VecInit(stations.map(_.ingress.free)).asUInt()

      sending := 0.U

      when(
        toIF.view(idx).invalAddr
        || toIF.view(idx).instr.base === InstrType.RESERVED
        || !applicables.orR()
      ) {
        // Is an invalid instruction
        selfCanIssue := stations(0).ingress.free && !taken(0)
        sending := 1.U
        // Run immediately
        instr.rs1ready := true.B
        instr.rs2ready := true.B
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

  inflights.writer.cnt := issueNum
  assert(inflights.writer.accept >= issueNum)
  inflights.writer.view := toIF.view.map(InflightInstr.from)

  val pendingBr = RegInit(false.B)
  val pendingBrTag = RegInit(0.U(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W))
  val pendingBrResult = RegInit(BranchResult.empty)
  assert(!pendingBr || pendingBrResult.branched())

  val brMask = Wire(Vec(units.size, UInt(coredef.INFLIGHT_INSTR_LIMIT.W)))
  val brMux = Wire(UInt(coredef.INFLIGHT_INSTR_LIMIT.W))
  brMux := brMask.reduceTree(_ | _) | Mux(pendingBr, pendingBrTag -% retirePtr, 0.U)
  val brSel = VecInit(PriorityEncoderOH(brMux.asBools())).asUInt()
  val brSeled = Wire(Vec(units.size, Bool()))
  val brNormlalized = Wire(Vec(units.size, new BranchResult))

  when(brSeled.asUInt.orR()) {
    pendingBr := true.B
    pendingBrTag := OHToUInt(brSel) +% retirePtr // Actually this is always true
    pendingBrResult := Mux1H(brSeled, brNormlalized)
  }

  // Filling ROB & CDB broadcast
  for(((u, ent), idx) <- units.zip(cdb.entries).zipWithIndex) {
    ent.valid := false.B
    ent.name := 0.U // Helps to debug, because we are asserting store(0) === 0
    ent.data := u.retire.info.wb

    // TODO: maybe pipeline here?
    val dist = u.retire.instr.tag -% retirePtr
    val oh = UIntToOH(dist)
    val normalized = u.retire.info.normalizedBranch(
      u.retire.instr.instr.instr.op,
      u.retire.instr.instr.taken,
      u.retire.instr.instr.npc
    )
    val canBr = !u.retire.instr.instr.vacant && normalized.branched()
    brMask(idx) := Mux(canBr, oh, 0.U)
    brSeled(idx) := brSel === oh && canBr
    brNormlalized(idx) := normalized

    when(!u.retire.instr.instr.vacant) {
      when(!u.retire.info.hasMem) {
        ent.name := u.retire.instr.rdname
        ent.valid := true.B
      }

      rob(u.retire.instr.tag).hasMem := u.retire.info.hasMem
      rob(u.retire.instr.tag).valid := true.B
    }
  }

  // Commit

  toBPU.valid := false.B
  toBPU.lpc := DontCare
  toBPU.taken := DontCare
  toBPU.hist := DontCare

  retireNum := 0.U

  // Default: no branch if nothing is retired
  toCtrl.branch.nofire()
  toCtrl.tval := DontCare
  toCtrl.nepc := DontCare

  cdb.entries(coredef.UNIT_COUNT) := DontCare
  cdb.entries(coredef.UNIT_COUNT).valid := false.B

  // TODO: send memory reqeust one tick before its turn

  when(!rob(retirePtr).valid) {
    // First one invalid, cannot retire anything
    retireNum := 0.U
    toCtrl.branch.nofire()
    for(rwp <- rw) {
      rwp.addr := 0.U
      rwp.data := DontCare
    }
  }.elsewhen(rob(retirePtr).hasMem) {
    // Is memory operation, wait for memAccSucc
    toCtrl.branch.nofire()
    for(rwp <- rw) {
      rwp.addr := 0.U
      rwp.data := DontCare
    }

    releaseMem.ready := !RegNext(releaseMem.fire())
    val memResult = RegNext(releaseMem.bits)
    val memFired = RegNext(releaseMem.fire())

    when(memFired) {
      retireNum := 1.U
      rob(retirePtr).valid := false.B
      retirePtr := retirePtr +% 1.U
      when(memResult.isLoad) {
        cdb.entries(coredef.UNIT_COUNT).name := retirePtr // tag === rdname
        cdb.entries(coredef.UNIT_COUNT).data := memResult.data
        cdb.entries(coredef.UNIT_COUNT).valid := true.B
        rw(0).addr := inflights.reader.view(0).erd
        rw(0).data := memResult.data
      }
    }.otherwise {
      retireNum := 0.U
    }
  }.otherwise {
    val blocked = Wire(Vec(coredef.RETIRE_NUM, Bool()))
    val isBranch = Wire(Vec(coredef.RETIRE_NUM, Bool()))

    val mask = blocked.asUInt
    val retireNumFast = Mux(
      mask === 0.U,
      coredef.RETIRE_NUM.U,
      OHToUInt(
        PriorityEncoderOH(
          blocked
        )
      )
    )
    retireNum := retireNumFast

    // First only not memory operation, possible to do multiple retirement
    // Compute if we can retire a certain instruction
    for(idx <- (0 until coredef.RETIRE_NUM)) {
      val inflight = inflights.reader.view(idx)
      val tag = retirePtr +% idx.U
      val info = rob(tag)

      isBranch(idx) := (
        pendingBr && pendingBrTag === tag
        || inflight.op === Decoder.Op("BRANCH").ident
      )

      if(idx == 0) {
        blocked(idx) := !info.valid
      } else {
        // Only allow mem ops in the first retire slot
        blocked(idx) := !info.valid || isBranch(idx-1) || info.hasMem
      }

      when(idx.U < retireNum) {
        rw(idx).addr := inflight.erd
        rw(idx).data := renamer.toExec.releases(idx).value

        // Update BPU accordingly
        when(
          inflight.op === Decoder.Op("BRANCH").ident
          // && info.info.branch.ex === ExReq.none
          // Update: BRANCH never exceptions
        ) {
          toBPU.valid := true.B
          toBPU.lpc := inflight.npc - 1.U
          toBPU.taken := pendingBr && pendingBrTag === tag
          toBPU.hist := inflight.pred
        }

        when(pendingBr && pendingBrTag === tag && pendingBrResult.ex =/= ExReq.none) {
          // Don't write-back exceptioned instr
          rw(idx).addr := 0.U
        }

        when(pendingBr && pendingBrTag === tag && pendingBrResult.ex === ExReq.ex) {
          toCtrl.tval := renamer.toExec.releases(idx).value
          toCtrl.nepc := inflight.addr
        }

        info.valid := false.B
      }.otherwise {
        rw(idx).addr := 0.U
        rw(idx).data := DontCare
      }
    }

    toCtrl.branch := BranchResult.empty
    when(pendingBr && pendingBrTag -% retirePtr < retireNumFast) {
      toCtrl.branch := pendingBrResult
    }
  }

  val retireNext = rob(retirePtr)

  // Renamer release
  for(i <- (0 until coredef.RETIRE_NUM)) {
    // renamer.toExec.releases(i).name := rob(retirePtr +% i.U).retirement.instr.rdname
    renamer.toExec.releases(i).name := retirePtr + i.U
    renamer.toExec.releases(i).reg := inflights.reader.view(i).erd
  }

  renamer.toExec.retire := retireNum
  inflights.reader.accept := retireNum
  assert(inflights.reader.cnt >= retireNum)

  // intAck
  when(retireNext.valid && retireNext.hasMem) {
    toCtrl.intAck := false.B
  }.otherwise {
    toCtrl.intAck := toCtrl.int
  }

  // Flushing control
  when(toCtrl.ctrl.flush) {
    issuePtr := 0.U
    retirePtr := 0.U
    pendingBr := false.B

    for(row <- rob) {
      row.valid := false.B
    }
  }
}

object Exec {
  class ROBEntry(implicit val coredef: CoreDef) extends Bundle {
    // val info = new RetireInfo
    val hasMem = Bool()
    val valid = Bool()
  }

  object ROBEntry {
    def empty(implicit coredef: CoreDef) = {
      val ret = Wire(new ROBEntry)
      ret.hasMem := DontCare
      ret.valid := false.B

      ret
    }
  }

  def route(instr: Instr)(implicit coredef: CoreDef): UInt = {
    val ret = Wire(UInt(coredef.UNIT_COUNT.W))
    ret := 0.U

    switch(instr.op) {
      is(Decoder.Op("LUI").ident, Decoder.Op("AUIPC").ident, Decoder.Op("JAL").ident) {
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
