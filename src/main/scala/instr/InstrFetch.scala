package instr

import chisel3._
import _root_.cache._
import Decoder._
import _root_.core._
import _root_.data._
import chisel3.util.log2Ceil
import chisel3.util.Queue
import chisel3.util.MuxLookup
import chisel3.util.MuxCase

class InstrExt(val ADDR_WIDTH: Int = 48) extends Bundle {
  val addr = UInt(ADDR_WIDTH.W)
  val instr = new Instr
  val vacant = Bool()

  override def toPrintable: Printable = {
    p"Address: 0x${Hexadecimal(addr)}\n" +
    p"Vacant: ${vacant}\n" +
    p"${instr}"
  }
}

object InstrExt {
  def empty(ADDR_WIDTH: Int): InstrExt = {
    val ret = Wire(new InstrExt(ADDR_WIDTH))

    ret.addr := DontCare
    ret.instr := DontCare
    ret.vacant := true.B

    ret
  }
}

class InstrFifoReader(val coredef: CoreDef) extends Bundle {
  val view = Input(Vec(coredef.ISSUE_NUM, new InstrExt(coredef.ADDR_WIDTH)))
  val cnt = Input(UInt(log2Ceil(coredef.ISSUE_NUM + 1).W))

  val pop = Output(UInt(log2Ceil(coredef.ISSUE_NUM + 1).W))
}

class InstrFetch(coredef: CoreDef) extends MultiIOModule {
  val toCtrl = IO(new Bundle {
    val pc = Input(UInt(coredef.ADDR_WIDTH.W))
    val skip = Input(UInt(log2Ceil(coredef.FETCH_NUM).W))

    val ctrl = StageCtrl.stage()
    val irst = Input(Bool())
  })

  val toIC = IO(Flipped(new ICPort(coredef.L1I)))
  val toExec = IO(Flipped(new InstrFifoReader(coredef)))

  val decoded = Wire(Vec(coredef.FETCH_NUM, new InstrExt(coredef.ADDR_WIDTH)))

  assume(1 << log2Ceil(coredef.ISSUE_FIFO_DEPTH) == coredef.ISSUE_FIFO_DEPTH)

  val issueFifo = RegInit(VecInit(Seq.fill(coredef.ISSUE_FIFO_DEPTH)(InstrExt.empty(coredef.ADDR_WIDTH))))
  val issueFifoHead = RegInit(0.U(log2Ceil(coredef.ISSUE_FIFO_DEPTH).W))
  val issueFifoTail = RegInit(0.U(log2Ceil(coredef.ISSUE_FIFO_DEPTH).W))

  val issueFifoSize = issueFifoTail -% issueFifoHead

  val issueFifoNearlyFull = issueFifoSize > (coredef.ISSUE_FIFO_DEPTH - coredef.FETCH_NUM * 2 - 1).U

  toIC.flush := toCtrl.ctrl.flush
  toIC.rst := toCtrl.irst

  toIC.addr := toCtrl.pc
  toIC.read := !issueFifoNearlyFull

  val proceed = (!toIC.vacant) && (!toIC.stall)
  toCtrl.ctrl.stall := toIC.stall || (issueFifoNearlyFull && !toCtrl.ctrl.flush)

  val tailFailed = RegInit(false.B)
  val tail = RegInit(0.U(Const.INSTR_MIN_WIDTH.W))

  val pipePc = RegInit(0.U(coredef.ADDR_WIDTH.W))
  val pipeSkip = RegInit(0.U)
  when(toCtrl.ctrl.flush || proceed) {
    pipePc := toCtrl.pc
    pipeSkip := toCtrl.skip

    /*
    printf(p"IF:\n================\n")
    printf(p"Skipped: ${pipeSkip}\n\n")
    printf(p"Fetched:\n\n")
    for(instr <- decoded) {
      printf(p"${instr}\n\n")
    }
    */
  }

  val vecView = toIC.data.asTypeOf(Vec(coredef.FETCH_NUM, UInt(Const.INSTR_MIN_WIDTH.W)))

  for(i <- (0 until coredef.FETCH_NUM)) {
    decoded(i).addr := pipePc + (i*Const.INSTR_MIN_WIDTH/8).U
    decoded(i).vacant := toIC.vacant || i.U < pipeSkip

    if(i == coredef.FETCH_NUM-1) {
      val (parsed, success) = vecView(i).tryAsInstr16
      decoded(i).instr := parsed

      when(!success) {
        decoded(i).vacant := true.B

        when(proceed) {
          tailFailed := true.B
          tail := vecView(i)
        }
      }.otherwise {
        when(proceed) {
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

  when(toCtrl.ctrl.flush) {
    tailFailed := false.B
  }

  // FIFO interface

  val cnts = Wire(Vec(coredef.FETCH_NUM, UInt(log2Ceil(coredef.FETCH_NUM+1).W)))

  for(i <- (0 until coredef.FETCH_NUM)) {
    val incr = Mux(decoded(i).vacant, 0.U, 1.U)
    cnts(i) := (if(i == 0) { incr } else { cnts(i-1) + incr })
  }

  val totCnt = cnts(coredef.FETCH_NUM-1)
  val sieved = Wire(Vec(coredef.FETCH_NUM, new InstrExt(coredef.ADDR_WIDTH)))
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

  // lastly, flush fifo
  when(toCtrl.ctrl.flush) {
    issueFifoHead := 0.U
    issueFifoTail := 0.U
  }
}
