package meowv64.exec
import chisel3._
import chisel3.util._
import meowv64.cache.DCFenceStatus
import meowv64.cache.DCReader
import meowv64.cache.DCWriter
import meowv64.cache.L1UCPort
import meowv64.core.CSRWriter
import meowv64.core.CoreDef
import meowv64.core.ExReq
import meowv64.core.PrivLevel
import meowv64.core.Satp
import meowv64.core.StageCtrl
import meowv64.core.Status
import meowv64.exec.Exec.ROBEntry
import meowv64.exec.units._
import meowv64.instr._
import meowv64.paging.TLBExt
import meowv64.reg._
import meowv64.util._

/** Out-of-order execution (Tomasulo's algorithm)
  *
  * First we check if instructions are eligible to be issues. Criteria include:
  *   - Target reservation station has free slots
  *   - Number of in-flight instructions haven't exceeded the limit. This limit
  *     affects our rob buffer length, as well as renamed reg tags' length
  *   - Issue FIFO is not depleted
  */
class Exec(implicit val coredef: CoreDef) extends Module {
  val toCtrl = IO(new Bundle {
    val ctrl = StageCtrl.stage()
    val retCnt = Output(UInt(log2Ceil(coredef.RETIRE_NUM + 1).W))
    val nepc = Output(UInt(coredef.XLEN.W))

    val branch = Output(new BranchResult)
    val tval = Output(UInt(coredef.XLEN.W))

    val int = Input(Bool())
    val intAck = Output(Bool())

    val priv = Input(PrivLevel())
    val status = Input(new Status)

    val tlbRst = Input(Bool())

    /** Update fflags
      */
    val fflags = Valid(UInt(5.W))
  })

  val toBPU = IO(new Bundle {

    /** Update BPU based on execution result
      */
    val valid = Output(Bool())
    val lpc = Output(UInt(coredef.XLEN.W))
    val taken = Output(Bool())
    val hist = Output(new BPUResult)
  })

  val toCore = IO(new Bundle {
    val satp = Input(new Satp)
    val ptw = new TLBExt

    // debug
    val rsFreeMask = Output(UInt(coredef.UNIT_COUNT.W))
  })

  val csrWriter = IO(new CSRWriter(coredef.XLEN))

  // We don't stall now
  toCtrl.ctrl.stall := false.B

  // Register file read/write port
  // for each register type
  val toRF = IO(new Bundle {
    val ports =
      MixedVec(for ((ty, width) <- coredef.REGISTER_TYPES) yield new Bundle {
        // three read ports per issued instruction
        val rr = Vec(coredef.ISSUE_NUM * 3, new RegReader(width))
        // one write port per retired instruction
        val rw = Vec(coredef.RETIRE_NUM, new RegWriter(width))
      })
  })

  val toIF = IO(new MultiQueueIO(new InstrExt, coredef.ISSUE_NUM))

  val toDC = IO(new Bundle {
    val r = new DCReader
    val w = new DCWriter(coredef.L1D)
    val fs = new DCFenceStatus(coredef.L1D)
    val u = new L1UCPort(coredef.L1D)
  })

  val cdb = Wire(new CDB)

  val renamer = Module(new Renamer)
  for (idx <- 0 until coredef.REGISTER_TYPES.length) {
    for (i <- (0 until coredef.ISSUE_NUM)) {
      renamer.ports(idx).rr(i)(0) <> toRF.ports(idx).rr(i * 3)
      renamer.ports(idx).rr(i)(1) <> toRF.ports(idx).rr(i * 3 + 1)
      renamer.ports(idx).rr(i)(2) <> toRF.ports(idx).rr(i * 3 + 2)
    }
    renamer.ports(idx).rw <> toRF.ports(idx).rw
  }
  renamer.cdb := cdb
  renamer.toExec.flush := toCtrl.ctrl.flush

  // Inflight instr info
  val inflights = Module(
    new MultiQueue(
      new InflightInstr,
      coredef.INFLIGHT_INSTR_LIMIT,
      coredef.ISSUE_NUM,
      coredef.RETIRE_NUM
    )
  )
  inflights.flush := toCtrl.ctrl.flush

  // Delayed memory ops
  val releaseMem = Wire(DeqIO(new DelayedMemResult))
  releaseMem.nodeq()

  // Units
  val lsu = Module(new LSU).suggestName("LSU");

  // collect execution units dynamically
  val units = for (seq <- coredef.EXECUTION_UNITS) yield {
    if (seq == Seq(ExecUnitType.lsu)) {
      lsu
    } else {
      val bypassIdx = seq.indexOf(ExecUnitType.bypass)
      Module(
        new UnitSel(
          for (ty <- seq) yield {
            ty match {
              case ExecUnitType.alu => Module(new ALU).suggestName("ALU")
              case ExecUnitType.branch =>
                Module(new Branch).suggestName("Branch")
              case ExecUnitType.csr => Module(new CSR).suggestName("CSR")
              case ExecUnitType.bypass =>
                Module(new Bypass).suggestName("Bypass")
              case ExecUnitType.mul => Module(new Mul).suggestName("Mul")
              case ExecUnitType.div => Module(new Div(16)).suggestName("Div")
              case ExecUnitType.fma => Module(new FMA).suggestName("FMA")
              case ExecUnitType.floatMisc =>
                Module(new FloatMisc).suggestName("FloatMisc")
              case ExecUnitType.fDivSqrt =>
                Module(new FDivSqrt).suggestName("FDivSqrt")
            }
          },
          instr => {
            seq.map({ ty =>
              instr.info.execUnit === ty
            })
          },
          bypassIdx = if (bypassIdx == -1) { None }
          else { Option(bypassIdx) },
          hasPipe = false
        )
      ),
    }
  }

  lsu.toMem.reader <> toDC.r
  lsu.toMem.writer <> toDC.w
  lsu.toMem.uncached <> toDC.u
  lsu.release <> releaseMem
  lsu.ptw <> toCore.ptw
  lsu.satp := toCore.satp
  lsu.priv := toCtrl.priv
  lsu.tlbRst := toCtrl.tlbRst
  lsu.status := toCtrl.status
  val hasPendingMem = lsu.hasPending

  // Connect extra ports
  units(0).extras("CSR") <> csrWriter
  units(0).extras("priv") := toCtrl.priv
  units(0).extras("status") := toCtrl.status

  assume(units.length == coredef.UNIT_COUNT)
  // TODO: asserts Bypass is in unit 0

  val stations = units.zipWithIndex.map({
    case (u, idx) => {
      // FIXME: do not hardcode 3
      val rs = if (idx != 3) {
        Module(new OoOResStation(idx)).suggestName(s"ResStation_${idx}")
      } else {
        assert(u.isInstanceOf[LSU])
        val lsb = Module(new LSBuf(idx)).suggestName(s"LSBuf")
        lsb.hasPending := hasPendingMem
        lsb.fs := toDC.fs
        lsb
      }
      rs.cdb := cdb
      rs.egress <> u.rs

      rs
    }
  })

  // collect rs free mask to find bottleneck
  toCore.rsFreeMask := Cat(stations.map(_.ingress.free))

  for (s <- stations) {
    s.cdb := cdb
    s.ctrl.flush := toCtrl.ctrl.flush

    // By default: nothing pushes
    s.ingress := DontCare
    s.ingress.push := false.B
  }

  for (u <- units) {
    u.flush := toCtrl.ctrl.flush
  }

  // ROB & ptrs
  val rob = RegInit(
    VecInit(Seq.fill(coredef.INFLIGHT_INSTR_LIMIT)(ROBEntry.empty))
  )
  val retirePtr = RegInit(0.U(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W))
  val issuePtr = RegInit(0.U(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W))

  val retireNum = Wire(UInt(log2Ceil(coredef.RETIRE_NUM + 1).W))
  val issueNum = Wire(UInt(log2Ceil(coredef.ISSUE_NUM + 1).W))

  when(retirePtr === issuePtr) {
    assert(inflights.reader.cnt === 0.U)
  }.otherwise {
    assert(inflights.reader.cnt =/= 0.U)
  }

  toIF.accept := issueNum
  renamer.toExec.commit := issueNum
  renamer.toExec.input := toIF.view
  renamer.toExec.ntag := issuePtr

  issuePtr := issuePtr + issueNum
  retirePtr := retirePtr + retireNum

  toCtrl.retCnt := retireNum

  // Issue
  val maxIssueNum =
    retirePtr -% issuePtr -% 1.U // issuePtr cannot reach retirePtr
  assert(issueNum <= maxIssueNum)

  val wasGFence = RegInit(false.B)
  val canIssue = Wire(Vec(coredef.ISSUE_NUM, Bool()))
  // this station is taken by previous instructions
  var taken = 0.U(coredef.UNIT_COUNT.W)
  issueNum := 0.U

  for (idx <- (0 until coredef.ISSUE_NUM)) {
    // whether this instruction can issue without considering previous instructions
    val selfCanIssue = Wire(Bool()).suggestName(s"selfCanIssue_$idx")
    // sending to which station
    val sending = Wire(UInt(coredef.UNIT_COUNT.W)).suggestName(s"sending_$idx")
    val instr = Wire(new ReservedInstr)

    // Is global fence? (FENCE.I, CSR)
    val isGFence = (
      instr.instr.instr.op === Decoder
        .Op("SYSTEM")
        .ident && instr.instr.instr.funct3 =/= Decoder.SYSTEM_FUNC("PRIV")
    )

    // At most only one sending
    assert(!(sending & (sending -% 1.U)).orR)
    assert(!selfCanIssue || sending.orR)

    instr := renamer.toExec.output(idx)

    when(
      idx.U >= toIF.cnt || !renamer.toExec.allowBit(idx) || idx.U >= maxIssueNum
    ) {
      selfCanIssue := false.B
      sending := 0.U
    }.otherwise {
      // Route to applicable stations
      val applicable = Exec.route(toIF.view(idx).instr)
      applicable.suggestName(s"applicable_$idx")
      // Find available stations
      val avails = VecInit(stations.map(_.ingress.free)).asUInt()
      avails.suggestName(s"avails_$idx")

      sending := 0.U

      when(
        toIF.view(idx).illegal
          || !applicable.orR()
      ) {
        // Is an illegal instruction
        // Forward to bypass unit
        selfCanIssue := stations(0).ingress.free && !taken(0)
        sending := 1.U
        // Run immediately
        instr.rs1ready := true.B
        instr.rs2ready := true.B
        instr.rs3ready := true.B
      }.elsewhen(wasGFence && issuePtr =/= retirePtr) {
        // GFence in-flight
        sending := DontCare
        selfCanIssue := false.B
        // TODO: only apply to first instr to optimize timing?
      }.otherwise {
        val mask = applicable & avails & ~taken
        mask.suggestName(s"mask_$idx")

        // Find lowest set
        sending := MuxCase(
          0.U,
          (0 until coredef.UNIT_COUNT).map(idx =>
            (
              mask(idx),
              (1 << idx).U
            )
          )
        )

        selfCanIssue := mask.orR()

        if (idx != 0) {
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

    if (idx == 0) canIssue(idx) := selfCanIssue
    else canIssue(idx) := selfCanIssue && canIssue(idx - 1)

    when(canIssue(idx)) {
      issueNum := (idx + 1).U

      for ((s, en) <- stations.zip(sending.asBools)) {
        when(en) {
          s.ingress.push := true.B
          s.ingress.instr := instr
        }
      }

      if (idx == 0) {
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
  val pendingBrTval = RegInit(0.U(coredef.XLEN.W))
  assert(!pendingBr || pendingBrResult.branched())

  // find the first branch instruction
  // branch here means control flow interruption (trap, missed prediction)
  val brMask = Wire(Vec(units.size, UInt(coredef.INFLIGHT_INSTR_LIMIT.W)))
  val brMux = Wire(UInt(coredef.INFLIGHT_INSTR_LIMIT.W))
  brMux := brMask.reduceTree(_ | _) | Mux(
    pendingBr,
    UIntToOH(pendingBrTag -% retirePtr),
    0.U
  )
  // index of first branch instruction
  val brSel = VecInit(PriorityEncoderOH(brMux.asBools())).asUInt()
  val brSeled = Wire(Vec(units.size, Bool()))
  // branch result and branch trap target
  val brResults = Wire(Vec(units.size, new BranchResult))
  val brTvals = Wire(Vec(units.size, UInt(coredef.XLEN.W)))

  when(brSeled.asUInt.orR()) {
    // a branch instruction is found
    pendingBr := true.B
    pendingBrTag := OHToUInt(brSel) +% retirePtr // Actually this is always true
    pendingBrResult := Mux1H(brSeled, brResults)
    pendingBrTval := Mux1H(brSeled, brTvals)
  }

  // Filling ROB & CDB broadcast
  for (((u, ent), idx) <- units.zip(cdb.entries).zipWithIndex) {
    ent.valid := false.B
    ent.name := 0.U // Helps to debug, because we are asserting store(0) === 0
    ent.data := u.retire.info.wb

    // TODO: maybe pipeline here?
    val dist = u.retire.instr.tag -% retirePtr
    val oh = UIntToOH(dist)
    val branchResult = u.retire.info.branch
    val canBr = u.retire.instr.instr.valid && branchResult.branched()
    brMask(idx) := Mux(canBr, oh, 0.U)
    brSeled(idx) := brSel === oh && canBr
    brResults(idx) := branchResult
    brTvals(idx) := u.retire.info.wb

    when(u.retire.instr.instr.valid) {
      when(!u.retire.info.hasMem) {
        ent.name := u.retire.instr.rdname
        ent.valid := true.B
      }

      rob(u.retire.instr.tag).hasMem := u.retire.info.hasMem
      rob(u.retire.instr.tag).valid := true.B
      // for BRANCH instructions, this means taken before normalization
      rob(u.retire.instr.tag).taken := u.retire.info.branchTaken
      rob(u.retire.instr.tag).updateFFlags := u.retire.info.updateFFlags
      rob(u.retire.instr.tag).fflags := u.retire.info.fflags
    }
  }

  // Commit

  toBPU.valid := false.B
  toBPU.lpc := DontCare
  toBPU.taken := DontCare
  toBPU.hist := DontCare

  retireNum := 0.U

  // Default: no branch if nothing is retired
  // nepc is set to the next retiring instruction for interrupts
  toCtrl.branch.nofire
  toCtrl.tval := DontCare
  toCtrl.nepc := inflights.reader.view(0).addr

  cdb.entries(coredef.UNIT_COUNT) := DontCare
  cdb.entries(coredef.UNIT_COUNT).valid := false.B

  // do not update fflags by default
  toCtrl.fflags.valid := false.B
  toCtrl.fflags.bits := 0.U

  // TODO: send memory request one tick before its turn

  val retireNext = rob(retirePtr)

  when(!retireNext.valid) {
    // First one invalid, cannot retire anything
    retireNum := 0.U
    for (i <- 0 until coredef.REGISTER_TYPES.length) {
      for (rwp <- toRF.ports(i).rw) {
        rwp.valid := false.B
        rwp.addr := 0.U
        rwp.data := 0.U
      }
    }
    toCtrl.branch := BranchResult.empty
  }.elsewhen(retireNext.hasMem) {
    // Is memory operation, wait for memAccSucc
    for (i <- 0 until coredef.REGISTER_TYPES.length) {
      for (rwp <- toRF.ports(i).rw) {
        rwp.valid := false.B
        rwp.addr := 0.U
        rwp.data := 0.U
      }
    }

    releaseMem.ready := !RegNext(releaseMem.fire)
    val memResult = RegNext(releaseMem.bits)
    val memFired = RegNext(releaseMem.fire)

    // For BPU mispredict on previous instructions
    toCtrl.branch := BranchResult.empty

    when(memFired) {
      retireNum := 1.U
      rob(retirePtr).valid := false.B
      retirePtr := retirePtr +% 1.U
      when(memResult.hasWB) {
        cdb.entries(coredef.UNIT_COUNT).name := retirePtr // tag === rdname
        cdb.entries(coredef.UNIT_COUNT).data := memResult.data
        cdb.entries(coredef.UNIT_COUNT).valid := true.B

        for (((ty, _), i) <- coredef.REGISTER_TYPES.zipWithIndex) {
          val rw = toRF.ports(i).rw

          when(inflights.reader.view(0).erd.ty === ty) {
            rw(0).valid := true.B
            rw(0).addr := inflights.reader.view(0).erd.index
            rw(0).data := memResult.data
          }
        }
      }

      when(pendingBr && pendingBrTag === retirePtr) {
        toCtrl.branch := pendingBrResult
      }
    }.otherwise {
      retireNum := 0.U
    }
  }.elsewhen(toCtrl.int && toCtrl.intAck) {
    // Interrupts inbound, retires nothing
    retireNum := 0.U
    for (i <- 0 until coredef.REGISTER_TYPES.length) {
      for (rwp <- toRF.ports(i).rw) {
        rwp.valid := false.B
        rwp.addr := 0.U
        rwp.data := 0.U
      }
    }

    toCtrl.branch := BranchResult.empty
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
    for (idx <- (0 until coredef.RETIRE_NUM)) {
      val inflight = inflights.reader.view(idx)
      val tag = retirePtr +% idx.U
      val info = rob(tag)

      isBranch(idx) := (
        pendingBr && pendingBrTag === tag
          || inflight.op === Decoder.Op("BRANCH").ident
      )

      if (idx == 0) {
        blocked(idx) := !info.valid
      } else {
        // Only allow mem ops in the first retire slot
        blocked(idx) := !info.valid || isBranch(idx - 1) || info.hasMem
      }

      when(idx.U < retireNum) {
        for (((ty, _), i) <- coredef.REGISTER_TYPES.zipWithIndex) {
          val rw = toRF.ports(i).rw
          when(inflight.erd.ty === ty) {
            rw(idx).valid := true.B
            rw(idx).addr := inflight.erd.index
            rw(idx).data := renamer.toExec.releases(idx).value
          }
        }

        // update fflags
        when(info.updateFFlags) {
          toCtrl.fflags.valid := true.B
          toCtrl.fflags.bits := info.fflags
        }

        // Update BPU accordingly
        when(
          inflight.op === Decoder.Op("BRANCH").ident ||
            inflight.op === Decoder.Op("JAL").ident
          // && info.info.branch.ex === ExReq.none
          // Update: BRANCH never exceptions
        ) {
          toBPU.valid := true.B
          toBPU.lpc := inflight.npc - 1.U
          //toBPU.taken := pendingBr && pendingBrTag === tag
          toBPU.taken := info.taken
          toBPU.hist := inflight.pred
        }

        when(
          pendingBr && pendingBrTag === tag && pendingBrResult.ex =/= ExReq.none
        ) {
          // Don't write-back exceptioned instr
          for (i <- 0 until coredef.REGISTER_TYPES.length) {
            val rw = toRF.ports(i).rw
            rw(idx).valid := false.B
            rw(idx).addr := 0.U
            rw(idx).data := 0.U
          }
        }

        when(
          pendingBr && pendingBrTag === tag && pendingBrResult.ex === ExReq.ex
        ) {
          toCtrl.tval := pendingBrTval
          toCtrl.nepc := inflight.addr
        }

        info.valid := false.B
      }.otherwise {
        for (i <- 0 until coredef.REGISTER_TYPES.length) {
          val rw = toRF.ports(i).rw
          rw(idx).valid := false.B
          rw(idx).addr := 0.U
          rw(idx).data := 0.U
        }
      }
    }

    toCtrl.branch := BranchResult.empty
    when(pendingBr && pendingBrTag -% retirePtr < retireNumFast) {
      toCtrl.branch := pendingBrResult
    }
  }

  // Renamer release
  for (i <- (0 until coredef.RETIRE_NUM)) {
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
    // We need to ensure that the address we are giving ctrl is valid
    // which is equivalent to the inflight queue being not empty
    toCtrl.intAck := retirePtr =/= issuePtr
  }

  // Flushing control
  when(toCtrl.ctrl.flush) {
    issuePtr := 0.U
    retirePtr := 0.U
    pendingBr := false.B

    for (row <- rob) {
      row.valid := false.B
    }
  }
}

object Exec {
  class ROBEntry(implicit val coredef: CoreDef) extends Bundle {
    // val info = new RetireInfo
    /** Has memory access
      */
    val hasMem = Bool()
    val valid = Bool()

    /** Branch has taken
      */
    val taken = Bool()

    /** Update fflags
      */
    val updateFFlags = Bool()
    val fflags = UInt(5.W)
  }

  object ROBEntry {
    def empty(implicit coredef: CoreDef) = {
      val ret = Wire(new ROBEntry)
      ret.hasMem := DontCare
      ret.valid := false.B
      ret.taken := false.B
      ret.updateFFlags := false.B
      ret.fflags := false.B

      ret
    }
  }

  def route(instr: Instr)(implicit coredef: CoreDef): UInt = {
    val ret = Wire(UInt(coredef.UNIT_COUNT.W))
    ret := 0.U

    // compute bitset from unit configuration
    for (ty <- ExecUnitType.all) {
      when(instr.info.execUnit === ty) {
        // compute bit mask
        var mask = 0
        for ((seq, idx) <- coredef.EXECUTION_UNITS.zipWithIndex) {
          if (seq.contains(ty)) {
            mask |= 1 << idx
          }
        }

        ret := mask.U(coredef.UNIT_COUNT.W)
      }
    }

    ret
  }
}
