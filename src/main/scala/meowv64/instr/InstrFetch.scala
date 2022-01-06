package meowv64.instr

import chisel3._
import meowv64.cache._
import Decoder._
import meowv64.core._
import meowv64.data._
import chisel3.util._
import chisel3.experimental.chiselName
import chisel3.experimental.ChiselEnum
import meowv64.util._
import meowv64.paging._

object FetchEx extends ChiselEnum {
  val none, invalAddr, pageFault = Value
}

class InstrExt(implicit val coredef: CoreDef) extends Bundle {
  val addr = UInt(coredef.XLEN.W)
  val instr = new Instr
  val vacant = Bool()
  val fetchEx = FetchEx()
  val acrossPageEx =
    Bool() // Exception happens on the second half of this instruction
  val pred = new BPUResult
  val forcePred = Bool() // RAS and missed branch
  val predTarget = UInt(coredef.XLEN.W) // For JALR handling

  override def toPrintable: Printable = {
    p"Address: 0x${Hexadecimal(addr)}\n" +
      p"Vacant: ${vacant}\n" +
      p"${instr}"
  }

  def npc: UInt = Mux(instr.base === InstrType.C, addr + 2.U, addr + 4.U)
  def taken: Bool = forcePred || pred.prediction === BHTPrediction.taken
}

object InstrExt {
  def empty(implicit coredef: CoreDef): InstrExt = {
    val ret = Wire(new InstrExt)

    ret.addr := DontCare
    ret.instr := DontCare
    ret.vacant := true.B
    ret.pred := DontCare
    ret.fetchEx := DontCare
    ret.acrossPageEx := DontCare
    ret.forcePred := DontCare
    ret.predTarget := DontCare

    ret
  }
}

@chiselName
class InstrFetch(implicit val coredef: CoreDef) extends MultiIOModule {
  val toCtrl = IO(new Bundle {
    val pc = Input(UInt(coredef.XLEN.W))

    val ctrl = StageCtrl.stage()
    val irst = Input(Bool())
    val tlbrst = Input(Bool())

    val priv = Input(PrivLevel())
  })

  val toIC = IO(Flipped(new ICPort(coredef.L1I)))
  val toExec = IO(Flipped(new MultiQueueIO(new InstrExt, coredef.ISSUE_NUM)))

  val toBPU = IO(new Bundle {
    val pc = Output(UInt(coredef.XLEN.W))
    val results = Input(
      Vec(coredef.L1I.TRANSFER_SIZE / Const.INSTR_MIN_WIDTH, new BPUResult)
    )
  })

  val toRAS = IO(new Bundle {
    val push = ValidIO(UInt(coredef.XLEN.W))
    val pop = Flipped(DecoupledIO(UInt(coredef.XLEN.W)))
  })

  val debug = IO(new Bundle {
    val pc = Output(UInt(coredef.XLEN.W))
  })

  val toCore = IO(new Bundle {
    val satp = Input(new Satp)
    val ptw = new TLBExt
  })

  val pc = RegInit(coredef.INIT_VEC.U(coredef.XLEN.W)) // This should be aligned
  val fpc = WireDefault(pc)
  pc := fpc
  val pipePc = RegInit(0.U(coredef.XLEN.W))
  val pipeFault = RegInit(false.B)

  val tlb = Module(new TLB)
  val requiresTranslate =
    toCore.satp.mode =/= SatpMode.bare && toCtrl.priv <= PrivLevel.S
  // TODO: this will cause the flush to be sent one more tick
  val readStalled = toIC.stall || (requiresTranslate && !tlb.query.ready)

  when(!readStalled) {
    pipePc := fpc
    pipeFault := tlb.query.fault

    when(toIC.read) {
      pc := fpc + (coredef.L1I.TRANSFER_SIZE / 8).U
    }
  }

  tlb.ptw <> toCore.ptw
  tlb.satp := toCore.satp
  tlb.query.vpn := fpc(47, 12)
  tlb.query.query := requiresTranslate && !toCtrl.ctrl.flush
  tlb.query.isModify := false.B
  tlb.query.mode := Mux(
    toCtrl.priv === PrivLevel.U,
    TLBLookupMode.U,
    TLBLookupMode.S
  )
  tlb.flush := toCtrl.tlbrst

  toBPU.pc := Mux(
    toIC.stall,
    RegNext(toBPU.pc),
    fpc
  ) // Perdict by virtual memory
  // TODO: flush BPU on context switch

  // First, push all IC readouts into a queue
  class ICData extends Bundle {
    val data = UInt(coredef.L1I.TRANSFER_SIZE.W)
    val addr = UInt(coredef.XLEN.W)
    val pred =
      Vec(coredef.L1I.TRANSFER_SIZE / Const.INSTR_MIN_WIDTH, new BPUResult)
    val fault = Bool()
  }

  val ICQueue = Module(new FlushableQueue(new ICData, 2, false, false))
  ICQueue.io.enq.bits.data := toIC.data
  ICQueue.io.enq.bits.addr := pipePc
  ICQueue.io.enq.bits.pred := toBPU.results
  ICQueue.io.enq.bits.fault := pipeFault
  ICQueue.io.enq.valid := (!toIC.stall && !toIC.vacant) || pipeFault

  val pipeSpecBr = Wire(Bool())

  val haltIC = ICQueue.io.count >= 1.U && !toCtrl.ctrl.flush && !pipeSpecBr
  val icAddr = WireDefault(fpc)
  val icRead = WireDefault(!haltIC)
  when(requiresTranslate) {
    icAddr := tlb.query.ppn ## fpc(11, 0)
    icRead := (!haltIC && tlb.query.ready) && !tlb.query.fault
  }
  toIC.read := icRead && !toCtrl.ctrl.flush
  toIC.addr := icAddr
  toIC.rst := toCtrl.ctrl.flush && toCtrl.irst

  val ICHead = Module(new FlushableSlot(new ICData, true, true))
  ICHead.io.enq <> ICQueue.io.deq
  ICHead.io.deq.nodeq() // Default

  val headPtr = RegInit(0.U(log2Ceil(coredef.L1I.TRANSFER_SIZE / 16).W))

  val decodeVec = Wire(Vec(coredef.L1I.TRANSFER_SIZE * 2 / 16, UInt(16.W)))
  decodeVec := (ICQueue.io.deq.bits.data ## ICHead.io.deq.bits.data)
    .asTypeOf(decodeVec)
  val joinedVec = Wire(Vec(coredef.L1I.TRANSFER_SIZE * 2 / 16 - 1, UInt(32.W)))
  for ((v, i) <- joinedVec.zipWithIndex) {
    v := decodeVec(i + 1) ## decodeVec(i)
  }
  // Scala ++ works in reverse order (lttle endian you may say?)
  val joinedPred = VecInit(ICHead.io.deq.bits.pred ++ ICQueue.io.deq.bits.pred)

  val decodable = Wire(Vec(coredef.FETCH_NUM, Bool()))
  val decodePtr = Wire(
    Vec(
      coredef.FETCH_NUM + 1,
      UInt(log2Ceil(coredef.L1I.TRANSFER_SIZE * 2 / 16).W)
    )
  )
  val decoded = Wire(Vec(coredef.FETCH_NUM, new InstrExt))
  val decodedRASPush = Wire(Vec(coredef.FETCH_NUM, Bool()))
  val decodedRASPop = Wire(Vec(coredef.FETCH_NUM, Bool()))
  decodePtr(0) := headPtr

  for (i <- 0 until coredef.FETCH_NUM) {
    val overflowed = decodePtr(
      i
    ) >= (coredef.L1I.TRANSFER_SIZE / 16 - Const.INSTR_MIN_WIDTH / 8).U

    when(!overflowed) {
      decodable(i) := ICHead.io.deq.valid
    }.otherwise {
      decodable(
        i
      ) := ICHead.io.deq.valid && ICQueue.io.deq.valid && ICHead.io.count =/= 0.U
    }

    val raw = joinedVec(decodePtr(i))
    val (instr, isInstr16) = raw.parseInstr()
    when(isInstr16) {
      decodePtr(i + 1) := decodePtr(i) + 1.U
    } otherwise {
      decodePtr(i + 1) := decodePtr(i) + 2.U
    }

    decoded(i).instr := instr
    val addr = ICHead.io.deq.bits.addr + (decodePtr(i) << log2Ceil(
      (Const.INSTR_MIN_WIDTH / 8)
    ))
    val acrossPage =
      !isInstr16 && addr(12, log2Ceil(Const.INSTR_MIN_WIDTH / 8)).andR()
    decoded(i).addr := addr

    // If an instruction span across the page border:
    // We need to consider fault/inval addr from both the previous page and the next page
    // This is done by checking if the ppn of the instruction is the same as the ppn of
    // ICHead

    decoded(i).fetchEx := FetchEx.none
    decoded(i).acrossPageEx := false.B
    assume(coredef.XLEN != coredef.VADDR_WIDTH)
    val headAddr = ICHead.io.deq.bits.addr
    val isInvalAddr = WireDefault(
      // Fetch cannot be uncached. We are also ignoring tlb.query.uncached
      headAddr(coredef.XLEN - 1, coredef.PADDR_WIDTH).asSInt() =/= headAddr(
        coredef.PADDR_WIDTH - 1
      ).asSInt()
    )

    when(requiresTranslate) {
      switch(toCore.satp.mode) {
        is(SatpMode.sv48) {
          isInvalAddr := headAddr(coredef.XLEN - 1, coredef.VADDR_WIDTH)
            .asSInt() =/= headAddr(coredef.VADDR_WIDTH - 1).asSInt()
        }

        is(SatpMode.sv39) {
          isInvalAddr := headAddr(coredef.XLEN - 1, coredef.VADDR_WIDTH - 9)
            .asSInt() =/= headAddr(coredef.VADDR_WIDTH - 10).asSInt()
        }
      }
    }

    when(ICHead.io.deq.bits.fault) {
      decoded(i).fetchEx := FetchEx.pageFault
    }.elsewhen(acrossPage && ICQueue.io.deq.bits.fault) {
      decoded(i).fetchEx := FetchEx.pageFault
      decoded(i).acrossPageEx := true.B
    }.elsewhen(isInvalAddr) {
      decoded(i).fetchEx := FetchEx.invalAddr
    }

    decoded(i).vacant := false.B
    val pred = joinedPred(decodePtr(i + 1) - 1.U)
    decoded(i).pred := pred
    decoded(i).forcePred := false.B

    decodedRASPop(i) := false.B
    decodedRASPush(i) := false.B

    when(instr.op === Decoder.Op("JAL").ident) {
      decoded(i).forcePred := true.B
      decodedRASPush(i) := instr.rd === 1.U || instr.rd === 5.U
    }.elsewhen(instr.op === Decoder.Op("JALR").ident) {
      decodedRASPush(i) := instr.rd === 1.U || instr.rd === 5.U
      val doPop =
        (instr.rs1 === 1.U || instr.rs1 === 5.U) && instr.rs1 =/= instr.rd
      decodedRASPop(i) := doPop

      when(doPop) {
        decoded(i).forcePred := toRAS.pop.valid
      }
    }.elsewhen(instr.op === Decoder.Op("BRANCH").ident) {
      when(instr.imm < 0.S && pred.prediction === BHTPrediction.missed) {
        decoded(i).forcePred := true.B
      }
    }

    decoded(i).predTarget := (decoded(i).instr.imm +% decoded(i).addr.asSInt())
      .asUInt()
    when(instr.op === Decoder.Op("JALR").ident) {
      decoded(i).predTarget := toRAS.pop.bits
    }
  }

  val issueFifo = Module(
    new MultiQueue(
      new InstrExt,
      coredef.ISSUE_FIFO_DEPTH,
      coredef.FETCH_NUM,
      coredef.ISSUE_NUM
    )
  )
  val steppings = Wire(
    Vec(coredef.FETCH_NUM + 1, UInt(log2Ceil(coredef.FETCH_NUM + 1).W))
  )
  val brokens = Wire(Vec(coredef.FETCH_NUM + 1, Bool()))
  steppings(0) := 0.U
  brokens(0) := false.B
  for (i <- (0 until coredef.FETCH_NUM)) {
    brokens(i + 1) := Mux(
      (i + 1).U <= issueFifo.writer.accept && decodable(i),
      brokens(i),
      true.B
    )
    if (i > 0) {
      when(decoded(i - 1).taken) {
        brokens(i + 1) := true.B
      }
    }
    steppings(i + 1) := Mux(brokens(i + 1), steppings(i), (i + 1).U)
  }

  val stepping = steppings(coredef.FETCH_NUM)

  // RAS push/pop
  toRAS.pop.ready := stepping =/= 0.U && decodedRASPop(stepping -% 1.U)
  toRAS.push.valid := stepping =/= 0.U && decodedRASPush(stepping -% 1.U)
  toRAS.push.bits := decoded(stepping -% 1.U).npc

  val nHeadPtr = decodePtr(stepping) // Expanded
  headPtr := nHeadPtr // Trim

  assume(nHeadPtr.getWidth == headPtr.getWidth + 1)
  assert(stepping <= issueFifo.writer.accept)
  when(nHeadPtr.head(1)(0)) {
    ICHead.io.deq.deq()
  }

  toExec <> issueFifo.reader
  issueFifo.writer.view := decoded
  issueFifo.writer.cnt := stepping

  toCtrl.ctrl.stall := haltIC || toIC.stall

  val pendingIRst = RegInit(false.B)
  val pendingTLBRst = RegInit(false.B)
  val pendingFlush = RegInit(false.B)

  ICQueue.io.flush := false.B
  ICHead.io.flush := false.B
  issueFifo.flush := false.B

  // Speculative branch
  val pipeStepping = RegNext(stepping)
  val pipeTaken = RegNext(VecInit(decoded.map(_.taken)))
  val pipeSpecBrTargets = RegNext(VecInit(decoded.map(_.predTarget)))
  val pipeSpecBrMask = pipeTaken.zipWithIndex.map({ case (taken, idx) =>
    idx.U < pipeStepping && taken
  })
  val pipeSpecBrTarget = MuxLookup(
    true.B,
    0.U,
    pipeSpecBrMask.zip(pipeSpecBrTargets)
  )
  pipeSpecBr := VecInit(pipeSpecBrMask).asUInt.orR && RegNext(
    !toCtrl.ctrl.flush && !pipeSpecBr
  )

  when(pipeSpecBr) {
    ICQueue.io.flush := true.B
    ICHead.io.flush := true.B
    // Do not push into issue fifo in this cycle
    issueFifo.writer.cnt := 0.U
    // Do not commit RAS
    toRAS.push.valid := false.B
    toRAS.pop.ready := false.B

    val ICAlign = log2Ceil(coredef.L1I.TRANSFER_SIZE / 8)
    fpc := pipeSpecBrTarget(coredef.XLEN - 1, ICAlign) ## 0.U(ICAlign.W)
    pipeFault := false.B
    headPtr := pipeSpecBrTarget(
      ICAlign - 1,
      log2Ceil(Const.INSTR_MIN_WIDTH / 8)
    )

    pendingIRst := false.B
    pendingTLBRst := toCtrl.tlbrst

    when(readStalled) {
      pendingFlush := true.B
    }
  }

  // Flushing

  when(toCtrl.ctrl.flush) {
    // External flushing, wait for one tick
    // This is to ensure priv level and other environment are set up correctly
    issueFifo.flush := true.B
    ICQueue.io.flush := true.B
    ICHead.io.flush := true.B
    toCtrl.ctrl.stall := false.B

    val ICAlign = log2Ceil(coredef.L1I.TRANSFER_SIZE / 8)
    // Set pc directly, because we are waiting for one tick
    pc := toCtrl.pc(coredef.XLEN - 1, ICAlign) ## 0.U(ICAlign.W)
    pipeFault := false.B
    headPtr := toCtrl.pc(ICAlign - 1, log2Ceil(Const.INSTR_MIN_WIDTH / 8))

    pendingIRst := toCtrl.irst
    pendingTLBRst := toCtrl.tlbrst

    when(readStalled) {
      pendingFlush := true.B
    }
  }

  // Flush on IC stall tick
  when(pendingFlush) {
    toIC.rst := pendingIRst
    tlb.flush := pendingTLBRst
    ICQueue.io.enq.noenq()
    when(!toIC.stall) { // TLB will flush within one tick
      pendingFlush := false.B
    }
  }

  // TODO: asserts that decodable is shaped like [true, ..., true, false, ..., false] when there is no BPU

  debug.pc := fpc
}
