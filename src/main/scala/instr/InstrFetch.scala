package instr

import chisel3._
import _root_.cache._
import Decoder._
import _root_.core._
import _root_.data._
import chisel3.util._
import _root_.util.FlushableQueue
import chisel3.experimental.chiselName

class InstrExt(val XLEN: Int = 64) extends Bundle {
  val addr = UInt(XLEN.W)
  val instr = new Instr
  val vacant = Bool()
  val invalAddr = Bool()
  val pred = Bool()

  override def toPrintable: Printable = {
    p"Address: 0x${Hexadecimal(addr)}\n" +
    p"Vacant: ${vacant}\n" +
    p"${instr}"
  }

  def npc: UInt = Mux(instr.base === InstrType.C, addr + 2.U, addr + 4.U)
}

object InstrExt {
  def empty(XLEN: Int): InstrExt = {
    val ret = Wire(new InstrExt(XLEN))

    ret.addr := DontCare
    ret.instr := DontCare
    ret.vacant := true.B
    ret.pred := DontCare
    ret.invalAddr := DontCare

    ret
  }
}

class InstrFifoReader(implicit val coredef: CoreDef) extends Bundle {
  val view = Input(Vec(coredef.ISSUE_NUM, new InstrExt(coredef.XLEN)))
  val cnt = Input(UInt(log2Ceil(coredef.ISSUE_NUM + 1).W))
  val accept = Output(UInt(log2Ceil(coredef.ISSUE_NUM + 1).W))
}

class InstrFifoWriter(implicit val coredef: CoreDef) extends Bundle {
  val view = Input(Vec(coredef.FETCH_NUM, new InstrExt(coredef.XLEN)))
  val cnt = Input(UInt(log2Ceil(coredef.FETCH_NUM + 1).W))
  val accept = Output(UInt(log2Ceil(coredef.FETCH_NUM + 1).W))
}

class IssueFIFO(implicit val coredef: CoreDef) extends MultiIOModule {
  val reader = IO(Flipped(new InstrFifoReader))
  val writer = IO(new InstrFifoWriter)
  val flush = IO(Input(Bool()))

  val CNT = Integer.max(coredef.FETCH_NUM, coredef.ISSUE_NUM)
  val SIZE = (coredef.ISSUE_FIFO_DEPTH.toDouble / CNT).ceil.toInt

  val queues = (0 until CNT).map(idx => {
    val mod = Module(new FlushableQueue(new InstrExt, SIZE))
    mod.suggestName(s"queue_$idx")
    mod
  })

  for(q <- queues) {
    q.io.flush := flush
  }

  val readies = PopCount(queues.map(_.io.enq.ready))
  val valids = PopCount(queues.map(_.io.deq.valid))
  reader.cnt := valids.min(coredef.ISSUE_NUM.U)
  writer.accept := readies.min(coredef.FETCH_NUM.U)

  val enqs = VecInit(queues.map(_.io.enq))
  val deqs = VecInit(queues.map(_.io.deq))

  for(enq <- enqs) enq.noenq()
  for(deq <- deqs) deq.nodeq()

  val wptr = RegInit(0.U(log2Ceil(CNT).W))
  val rptr = RegInit(0.U(log2Ceil(CNT).W))

  for(i <- (0 until coredef.FETCH_NUM)) {
    when(writer.cnt > i.U) {
      enqs(wptr + i.U).enq(writer.view(i))
      assert(enqs(wptr + i.U).fire())
    }
  }

  for(i <- (0 until coredef.ISSUE_NUM)) {
    reader.view(i) := deqs(rptr + i.U).bits
    when(reader.accept > i.U) {
      deqs(rptr + i.U).deq()
      assert(deqs(rptr + i.U).fire())
    }
  }

  rptr := rptr +% reader.accept
  wptr := wptr +% writer.cnt

  when(flush) {
    wptr := 0.U
    rptr := 0.U
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
  val toExec = IO(Flipped(new InstrFifoReader))

  val toBPU = IO(new Bundle {
    val pc = Output(UInt(coredef.XLEN.W))
    val taken = Input(Vec(coredef.FETCH_NUM, BHTPerdiction()))
  })

  val debug = IO(new Bundle {
    val pc = Output(UInt(coredef.XLEN.W))
  })
  
  val pc = RegInit(coredef.INIT_VEC.U(coredef.XLEN.W)) // This should be aligned
  val pipePc = RegInit(0.U(coredef.XLEN.W))
  debug.pc := pc
  when(!toIC.stall) {
    pipePc := pc

    when(toIC.read) {
      pc := pc + (coredef.L1I.TRANSFER_SIZE / 8).U
    }
  }

  // First, push all IC readouts into a queue
  class ICData extends Bundle {
    val data = UInt(coredef.L1I.TRANSFER_SIZE.W)
    val addr = UInt(coredef.XLEN.W)
  }

  val ICQueue = Module(new FlushableQueue(new ICData, 2, false, true))
  ICQueue.io.enq.bits.data := toIC.data
  ICQueue.io.enq.bits.addr := pipePc
  ICQueue.io.enq.valid := !toIC.stall && !toIC.vacant
  
  val haltIC = ICQueue.io.count > 1.U
  toIC.read := !haltIC
  toIC.addr := pc
  toIC.rst := toCtrl.ctrl.flush && toCtrl.irst

  val ICHead = Module(new FlushableQueue(new ICData, 1, true, true))
  ICHead.io.enq <> ICQueue.io.deq
  ICHead.io.deq.nodeq() // Default

  val headPtr = RegInit(0.U(log2Ceil(coredef.L1I.TRANSFER_SIZE / 16).W))

  val decodeVec = Wire(Vec(coredef.L1I.TRANSFER_SIZE * 2 / 16, UInt(16.W)))
  decodeVec := (ICQueue.io.deq.bits.data ## ICHead.io.deq.bits.data).asTypeOf(decodeVec)
  val joinedVec = Wire(Vec(coredef.L1I.TRANSFER_SIZE * 2 / 16 - 1, UInt(32.W)))
  for((v, i) <- joinedVec.zipWithIndex) {
    v := decodeVec(i+1) ## decodeVec(i)
  }

  val decodable = Wire(Vec(coredef.FETCH_NUM, Bool()))
  val decodePtr = Wire(Vec(coredef.FETCH_NUM + 1, UInt(log2Ceil(coredef.L1I.TRANSFER_SIZE * 2 / 16).W)))
  val decoded = Wire(Vec(coredef.FETCH_NUM, new InstrExt))
  decodePtr(0) := headPtr

  // FIXME: BPU
  for(i <- 0 until coredef.FETCH_NUM) {
    val overflowed = decodePtr(i) >= (coredef.L1I.TRANSFER_SIZE / 16 - Const.INSTR_MIN_WIDTH / 8).U

    when(!overflowed) {
      decodable(i) := ICHead.io.deq.valid
    }.otherwise {
      decodable(i) := ICHead.io.deq.valid && ICQueue.io.deq.valid
    }

    val instr = joinedVec(decodePtr(i)).asInstr()
    when(instr.base === InstrType.C) {
      decodePtr(i+1) := decodePtr(i) + 1.U
    } otherwise {
      decodePtr(i+1) := decodePtr(i) + 2.U
    }

    decoded(i).instr := instr
    decoded(i).addr := ICHead.io.deq.bits.addr + (decodePtr(i) << log2Ceil((Const.INSTR_MIN_WIDTH / 8)))
    decoded(i).invalAddr := false.B
    decoded(i).vacant := false.B
    decoded(i).pred := false.B
  }

  val issueFifo = Module(new IssueFIFO)
  val steppings = Wire(Vec(coredef.FETCH_NUM + 1, UInt(log2Ceil(coredef.FETCH_NUM+1).W)))
  val brokens = Wire(Vec(coredef.FETCH_NUM + 1, Bool()))
  steppings(0) := 0.U
  brokens(0) := false.B
  for(i <- (0 until coredef.FETCH_NUM)) {
    brokens(i+1) := Mux((i+1).U <= issueFifo.writer.accept && decodable(i), brokens(i), true.B)
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

  // Special case: flushing
  issueFifo.flush := toCtrl.ctrl.flush
  ICQueue.io.flush := toCtrl.ctrl.flush
  ICHead.io.flush := toCtrl.ctrl.flush
  toCtrl.ctrl.stall := haltIC || toIC.stall

  val pendingIRST = RegInit(false.B)
  val pendingFlush = RegInit(false.B)

  when(toCtrl.ctrl.flush) {
    val ICAlign = log2Ceil(coredef.L1I.TRANSFER_SIZE / 8)
    pc := toCtrl.pc(coredef.XLEN-1, ICAlign)
    headPtr := toCtrl.pc(ICAlign-1, log2Ceil(Const.INSTR_MIN_WIDTH / 8))

    when(toIC.stall) {
      pendingFlush := true.B
      pendingIRST := toCtrl.irst
    }
  }

  when(pendingFlush) {
    toIC.rst := pendingIRST
    ICQueue.io.enq.noenq()

    when(!toIC.stall) {
      pendingFlush := false.B
    }
  }
  
  // TODO: impl BPU
  toBPU.pc := DontCare

  // TODO: asserts that decodable is shaped like [true, ..., true, false, ..., false] when there is no BPU

  /*
  val decoded = Wire(Vec(coredef.FETCH_NUM, new InstrExt(coredef.XLEN)))

  assume(1 << log2Ceil(coredef.ISSUE_FIFO_DEPTH) == coredef.ISSUE_FIFO_DEPTH)

  val issueFifo = RegInit(VecInit(Seq.fill(coredef.ISSUE_FIFO_DEPTH)(InstrExt.empty(coredef.XLEN))))
  val issueFifoHead = RegInit(0.U(log2Ceil(coredef.ISSUE_FIFO_DEPTH).W))
  val issueFifoTail = RegInit(0.U(log2Ceil(coredef.ISSUE_FIFO_DEPTH).W))

  val issueFifoSize = issueFifoTail -% issueFifoHead

  val issueFifoNearlyFull = issueFifoSize > (coredef.ISSUE_FIFO_DEPTH - coredef.FETCH_NUM * 2 - 1).U

  val flushed = RegInit(false.B)
  val flushedRst = RegInit(false.B)
  val flushedJmpTo = RegInit(0.U(coredef.VADDR_WIDTH.W))

  toIC.addr := Mux(flushed, flushedJmpTo, toCtrl.pc)
  toIC.rst := Mux(flushed, flushedRst, toCtrl.irst)
  toIC.read := (!issueFifoNearlyFull) || toCtrl.ctrl.flush

  // issueFifoNearlyFull only halts IC, but doesn't stop the decoder
  val proceed = !toIC.stall
  toCtrl.ctrl.stall := !toCtrl.ctrl.flush && (toIC.stall || issueFifoNearlyFull || flushed)

  when(toCtrl.ctrl.flush) {
    when(toIC.stall) {
      flushed := true.B
      flushedJmpTo := toCtrl.pc
      flushedRst := toCtrl.irst
    }
  }.elsewhen(!toIC.stall) {
    flushed := false.B
  }

  val tailFailed = RegInit(false.B)
  val tail = RegInit(0.U(Const.INSTR_MIN_WIDTH.W))

  val pipePc = RegInit(coredef.INIT_VEC.U(coredef.XLEN.W))
  val pipeNpc = RegInit(coredef.INIT_VEC.U(coredef.XLEN.W))
  val pipeSkip = RegInit(0.U(coredef.XLEN.W))

  when(!toCtrl.ctrl.stall) {
    pipePc := toCtrl.pc
    pipeSkip := toCtrl.skip
    pipeNpc := toCtrl.pc + (Const.INSTR_MIN_WIDTH / 8 * coredef.FETCH_NUM).U

    /*
    printf(p"IF:\n================\n")
    printf(p"Skipped: ${pipeSkip}\n\n")
    printf(p"Fetched:\n\n")
    for(instr <- decoded) {
      printf(p"${instr}\n\n")
    }
     */
  }

  // Sync BPU and IC
  toBPU.pc := Mux(toCtrl.ctrl.stall, pipePc, toCtrl.pc)

  val vecView = toIC.data.asTypeOf(Vec(coredef.FETCH_NUM, UInt(Const.INSTR_MIN_WIDTH.W)))

  // Compute perdicted PC
  for((instr, pred) <- decoded.zip(toBPU.taken)) {
    instr.pred := DontCare

    switch(pred) {
      is(BHTPerdiction.taken) {
        instr.pred := true.B
      }

      is(BHTPerdiction.notTaken) {
        instr.pred := false.B
      }

      is(BHTPerdiction.missed) {
        instr.pred := instr.instr.op === Decoder.Op("BRANCH").ident && instr.instr.imm < 0.S
      }
    }

    // If is JAL, then force perdicted
    when(instr.instr.op === Decoder.Op("JAL").ident) {
      instr.pred := true.B
    }
  }

  toCtrl.perdicted := MuxCase(
    pipeNpc,
    decoded.zipWithIndex.map({ case (instr, idx) => {
      (instr.pred && !instr.vacant, (instr.addr.asSInt +% instr.instr.imm).asUInt) // B-type instr
    }})
  )

  val arrived = RegInit(false.B)
  val arrivedStash = RegInit(0.U(coredef.XLEN.W))

  when((!toCtrl.ctrl.stall) || toIC.read && !toIC.stall) {
    arrived := false.B
  }.elsewhen(!toIC.stall && !toIC.vacant && toCtrl.ctrl.stall) {
    arrived := true.B
    arrivedStash := toCtrl.perdicted
  }

  when(arrived) {
    toCtrl.perdicted := arrivedStash
  }

  for(i <- (0 until coredef.FETCH_NUM)) {
    val addr = pipePc + (i*Const.INSTR_MIN_WIDTH/8).U
    decoded(i).addr := addr
    // Instr skipped because of fetch pipeline events
    val fetchVacant = toIC.vacant || i.U < pipeSkip || flushed
    decoded(i).vacant := fetchVacant
    val invalAddr = if(coredef.XLEN == coredef.VADDR_WIDTH) {
      false.B
    } else {
      addr(coredef.XLEN-1, coredef.VADDR_WIDTH).orR()
    }
    decoded(i).invalAddr := invalAddr

    if(i == coredef.FETCH_NUM-1) {
      val (parsed, success) = vecView(i).tryAsInstr16
      decoded(i).instr := parsed

      when(!success) {
        decoded(i).vacant := true.B

        when(proceed && !fetchVacant) {
          tailFailed := true.B
          tail := vecView(i)
        }
      }.otherwise {
        when(proceed && !fetchVacant) {
          tailFailed := false.B
        }
      }
    } else if(i == 0) {
      when(tailFailed) {
        val full = vecView(0) ## tail
        decoded(0).instr := full.asInstr
        decoded(0).addr := pipePc - (Const.INSTR_MIN_WIDTH/8).U
      }.otherwise {
        val full = vecView(i+1) ## vecView(i)
        decoded(i).instr := full.asInstr
      }
    } else {
      // Fuse two instructions together
      val full = vecView(i+1) ## vecView(i)
      decoded(i).instr := full.asInstr
    }

    // Skip this instr if last one was a full instr
    if(i != 0) {
      var cond = decoded(i-1).instr.base =/= InstrType.toInt(InstrType.C) && !decoded(i-1).vacant
      if(i == 1) cond = cond && !tailFailed
      when(cond) {
        decoded(i).vacant := true.B

        if(i == coredef.FETCH_NUM-1) {
          when(proceed) {
            tailFailed := false.B
          }
        }
      }
    }
  }

  var branched = false.B
  for(instr <- decoded) {
    when(branched) {
      instr.vacant := true.B
    }

    // TODO: optimize timing
    branched = branched || (!instr.vacant) && instr.pred
  }

  when(branched) {
    tailFailed := false.B
  }

  // FIFO interface

  val cnts = Wire(Vec(coredef.FETCH_NUM, UInt(log2Ceil(coredef.FETCH_NUM+1).W)))

  for(i <- (0 until coredef.FETCH_NUM)) {
    val incr = Mux(decoded(i).vacant, 0.U, 1.U)
    cnts(i) := (if(i == 0) { incr } else { cnts(i-1) + incr })
  }

  val totCnt = cnts(coredef.FETCH_NUM-1)
  val sieved = Wire(Vec(coredef.FETCH_NUM, new InstrExt(coredef.XLEN)))
  for(i <- (0 until coredef.FETCH_NUM)) {
    sieved(i) := MuxCase(DontCare, cnts.zip(decoded).map(_ match {
      case (cnt, instr) => ((!instr.vacant) && cnt === (i+1).U, instr)
    }))
  }

  // Push into FIFO
  when(proceed) {
    for((instr, idx) <- sieved.zipWithIndex) {
      when(idx.U < totCnt) {
        // issue fifo should never overflow
        assert(issueFifoTail +% idx.U =/= (issueFifoHead-%1.U))
        issueFifo(issueFifoTail +% idx.U) := instr
      }
    }

    issueFifoTail := issueFifoTail +% totCnt
  }

  // Connect toExec
  for((instr, idx) <- toExec.view.zipWithIndex) {
    instr := issueFifo(issueFifoHead + idx.U)
  }

  when(issueFifoSize >= coredef.FETCH_NUM.U) {
    toExec.cnt := coredef.FETCH_NUM.U
  }.otherwise {
    toExec.cnt := issueFifoSize
  }

  issueFifoHead := issueFifoHead +% toExec.pop

  // Flush operations, we put them here to override everything

  when(toCtrl.ctrl.flush) {
    tailFailed := false.B
    issueFifoHead := 0.U
    issueFifoTail := 0.U
  }
  */
}
