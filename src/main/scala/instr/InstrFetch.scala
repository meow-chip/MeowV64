package instr

import chisel3._
import _root_.cache._
import Decoder._
import _root_.core._
import _root_.data._
import chisel3.util._
import chisel3.experimental.chiselName
import _root_.util._
import paging.TLB
import paging.TLBExt

class InstrExt(implicit val coredef: CoreDef) extends Bundle {
  val addr = UInt(coredef.XLEN.W)
  val instr = new Instr
  val vacant = Bool()
  val invalAddr = Bool()
  val pred = new BPUResult
  val forcePred = Bool() // RAS and missed branch

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
    ret.invalAddr := DontCare
    ret.forcePred := DontCare

    ret
  }
}

@chiselName
class InstrFetch(implicit val coredef: CoreDef) extends MultiIOModule {
  val toCtrl = IO(new Bundle {
    val pc = Input(UInt(coredef.XLEN.W))

    val ctrl = StageCtrl.stage()
    val irst = Input(Bool())
  })

  val toIC = IO(Flipped(new ICPort(coredef.L1I)))
  val toExec = IO(Flipped(new MultiQueueIO(new InstrExt, coredef.ISSUE_NUM)))

  val toBPU = IO(new Bundle {
    val pc = Output(UInt(coredef.XLEN.W))
    val results = Input(Vec(coredef.L1I.TRANSFER_SIZE / Const.INSTR_MIN_WIDTH, new BPUResult))
  })

  val debug = IO(new Bundle {
    val pc = Output(UInt(coredef.XLEN.W))
  })

  val toCore = IO(new Bundle {
    val satp = Input(new Satp)
    val ptw = new TLBExt
  })
  
  val pc = RegInit(coredef.INIT_VEC.U(coredef.XLEN.W)) // This should be aligned
  val pipePc = RegInit(0.U(coredef.XLEN.W))
  when(!toIC.stall) {
    pipePc := pc

    when(toIC.read) {
      pc := pc + (coredef.L1I.TRANSFER_SIZE / 8).U
    }
  }

  val tlb = Module(new TLB)
  tlb.ptw <> toCore.ptw
  tlb.satp := toCore.satp
  tlb.query.vpn := pc(47, 12) // TODO: judge on SATP
  tlb.query.query := toCore.satp.mode =/= SatpMode.bare

  toBPU.pc := RegNext(pc) // Perdict by virtual memory
  // TODO: flush BPU on context switch

  // TODO: BHT

  // First, push all IC readouts into a queue
  class ICData extends Bundle {
    val data = UInt(coredef.L1I.TRANSFER_SIZE.W)
    val addr = UInt(coredef.XLEN.W)
    val pred = Vec(coredef.L1I.TRANSFER_SIZE / Const.INSTR_MIN_WIDTH, new BPUResult)
  }

  val ICQueue = Module(new FlushableQueue(new ICData, 2, false, false))
  ICQueue.io.enq.bits.data := toIC.data
  ICQueue.io.enq.bits.addr := pipePc
  ICQueue.io.enq.bits.pred := toBPU.results
  ICQueue.io.enq.valid := !toIC.stall && !toIC.vacant

  val pipeSpecBr = Wire(Bool())

  val haltIC = ICQueue.io.count >= 1.U && !toCtrl.ctrl.flush && !pipeSpecBr
  val fpc = WireDefault(pc)
  val icAddr = WireDefault(fpc)
  val icRead = WireDefault(!haltIC)
  when(toCore.satp.mode =/= SatpMode.bare) {
    icAddr := tlb.query.ppn ## fpc(11, 0)
    icRead := !haltIC && tlb.query.ready
  }
  toIC.read := icRead
  toIC.addr := icAddr
  toIC.rst := toCtrl.ctrl.flush && toCtrl.irst

  val ICHead = Module(new FlushableSlot(new ICData, true, true))
  ICHead.io.enq <> ICQueue.io.deq
  ICHead.io.deq.nodeq() // Default

  val headPtr = RegInit(0.U(log2Ceil(coredef.L1I.TRANSFER_SIZE / 16).W))

  val decodeVec = Wire(Vec(coredef.L1I.TRANSFER_SIZE * 2 / 16, UInt(16.W)))
  decodeVec := (ICQueue.io.deq.bits.data ## ICHead.io.deq.bits.data).asTypeOf(decodeVec)
  val joinedVec = Wire(Vec(coredef.L1I.TRANSFER_SIZE * 2 / 16 - 1, UInt(32.W)))
  for((v, i) <- joinedVec.zipWithIndex) {
    v := decodeVec(i+1) ## decodeVec(i)
  }
  // Scala ++ works in reverse order (lttle endian you may say?)
  val joinedPred = VecInit(ICHead.io.deq.bits.pred ++ ICQueue.io.deq.bits.pred)

  val decodable = Wire(Vec(coredef.FETCH_NUM, Bool()))
  val decodePtr = Wire(Vec(coredef.FETCH_NUM + 1, UInt(log2Ceil(coredef.L1I.TRANSFER_SIZE * 2 / 16).W)))
  val decoded = Wire(Vec(coredef.FETCH_NUM, new InstrExt))
  decodePtr(0) := headPtr

  for(i <- 0 until coredef.FETCH_NUM) {
    val overflowed = decodePtr(i) >= (coredef.L1I.TRANSFER_SIZE / 16 - Const.INSTR_MIN_WIDTH / 8).U

    when(!overflowed) {
      decodable(i) := ICHead.io.deq.valid
    }.otherwise {
      // FIXME: break at page border, so we can handle access errors & inval addr more conviently
      decodable(i) := ICHead.io.deq.valid && ICQueue.io.deq.valid && ICHead.io.count =/= 0.U
    }

    val raw = joinedVec(decodePtr(i))
    val (instr, isInstr16) = raw.parseInstr()
    when(isInstr16) {
      decodePtr(i+1) := decodePtr(i) + 1.U
    } otherwise {
      decodePtr(i+1) := decodePtr(i) + 2.U
    }

    decoded(i).instr := instr
    decoded(i).addr := ICHead.io.deq.bits.addr + (decodePtr(i) << log2Ceil((Const.INSTR_MIN_WIDTH / 8)))
    decoded(i).invalAddr := (if(coredef.XLEN == coredef.VADDR_WIDTH) {
      false.B
    } else {
      // TODO: check PADDR_WIDTH when SATP mode === no traslation
      ICHead.io.deq.bits.addr(coredef.XLEN-1, coredef.VADDR_WIDTH).orR
    })
    decoded(i).vacant := false.B
    val pred = joinedPred(decodePtr(i+1) - 1.U)
    decoded(i).pred := pred
    decoded(i).forcePred := false.B

    when(instr.op === Decoder.Op("JAL").ident) {
      decoded(i).forcePred := true.B
    }.elsewhen(instr.op === Decoder.Op("BRANCH").ident) {
      when(instr.imm < 0.S && pred.prediction === BHTPrediction.missed) {
        decoded(i).forcePred := true.B
      }
    }
  }

  val issueFifo = Module(new MultiQueue(new InstrExt, coredef.ISSUE_FIFO_DEPTH, coredef.FETCH_NUM, coredef.ISSUE_NUM))
  val steppings = Wire(Vec(coredef.FETCH_NUM + 1, UInt(log2Ceil(coredef.FETCH_NUM+1).W)))
  val brokens = Wire(Vec(coredef.FETCH_NUM + 1, Bool()))
  steppings(0) := 0.U
  brokens(0) := false.B
  for(i <- (0 until coredef.FETCH_NUM)) {
    brokens(i+1) := Mux(
      (i+1).U <= issueFifo.writer.accept && decodable(i),
      brokens(i),
      true.B
    )
    if(i > 0) {
      when(decoded(i-1).taken) {
        brokens(i+1) := true.B
      }
    }
    steppings(i+1) := Mux(brokens(i+1), steppings(i), (i+1).U)
  }

  val stepping = steppings(coredef.FETCH_NUM)

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

  val pendingIRST = RegInit(false.B)
  val pendingFlush = RegInit(false.B)

  ICQueue.io.flush := false.B
  ICHead.io.flush := false.B
  issueFifo.flush := false.B

  // Speculative branch
  val pipeStepping = RegNext(stepping)
  val pipeTaken = RegNext(VecInit(decoded.map(_.taken)))
  val pipeAddrs = RegNext(VecInit(decoded.map(_.addr.asSInt())))
  val pipeImms = RegNext(VecInit(decoded.map(_.instr.imm)))
  val pipeSpecBrTargets = pipeAddrs.zip(pipeImms).map({ case (addr, imm) => (addr +% imm).asUInt })
  val pipeSpecBrMask = pipeTaken.zipWithIndex.map({ case (taken, idx) => idx.U < pipeStepping && taken })
  val pipeSpecBrTarget = MuxLookup(
    true.B,
    0.U,
    pipeSpecBrMask.zip(pipeSpecBrTargets)
  )
  pipeSpecBr := VecInit(pipeSpecBrMask).asUInt.orR && RegNext(!toCtrl.ctrl.flush && !pipeSpecBr)

  when(pipeSpecBr) {
    pendingIRST := false.B

    ICQueue.io.flush := true.B
    ICHead.io.flush := true.B
    // Do not push into issue fifo in this cycle
    issueFifo.writer.cnt := 0.U

    val ICAlign = log2Ceil(coredef.L1I.TRANSFER_SIZE / 8)
    headPtr := pipeSpecBrTarget(ICAlign-1, log2Ceil(Const.INSTR_MIN_WIDTH / 8))
    val rawbpc = pipeSpecBrTarget(coredef.XLEN-1, ICAlign) ## 0.U(ICAlign.W)
    val bpc = WireDefault(rawbpc)

    when(toIC.stall) {
      pendingIRST := false.B
      pendingFlush := true.B
    }.otherwise {
      bpc := rawbpc + (1.U << ICAlign)
    }

    pc := bpc
    fpc := rawbpc
    toBPU.pc := rawbpc
    when(!toIC.stall) {
      pipePc := rawbpc
    }
  }

  // Flushing
  when(toCtrl.ctrl.flush) {
    issueFifo.flush := true.B
    ICQueue.io.flush := true.B
    ICHead.io.flush := true.B
    toCtrl.ctrl.stall := false.B
  }

  when(toCtrl.ctrl.flush) {
    val ICAlign = log2Ceil(coredef.L1I.TRANSFER_SIZE / 8)
    val rawbpc = toCtrl.pc(coredef.XLEN-1, ICAlign) ## 0.U(ICAlign.W)
    val bpc = WireDefault(rawbpc)

    when(toIC.stall) {
      pendingFlush := true.B
      pendingIRST := toCtrl.irst
    }.otherwise {
      bpc := rawbpc + (1.U << ICAlign)
    }

    pc := bpc
    headPtr := toCtrl.pc(ICAlign-1, log2Ceil(Const.INSTR_MIN_WIDTH / 8))

    fpc := rawbpc
    toBPU.pc := rawbpc
    when(!toIC.stall) {
      pipePc := rawbpc
    }
  }

  when(pendingFlush) {
    toIC.rst := pendingIRST
    ICQueue.io.enq.noenq()
    when(!toIC.stall) {
      pendingFlush := false.B
    }
  }
  
  toBPU.pc := DontCare

  // TODO: asserts that decodable is shaped like [true, ..., true, false, ..., false] when there is no BPU

  debug.pc := toIC.addr
}
