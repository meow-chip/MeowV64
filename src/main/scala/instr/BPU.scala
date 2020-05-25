package instr

import chisel3._
import _root_.cache._
import _root_.core._
import _root_.data._
import chisel3.util._
import chisel3.experimental.ChiselEnum

object BHTPrediction extends ChiselEnum {
  val taken, notTaken, missed = Value
}

class BHTSlot(implicit val coredef: CoreDef) extends Bundle {
  val OFFSET_WIDTH = log2Ceil(coredef.L1I.TRANSFER_SIZE)
  val INDEX_WIDTH = log2Ceil(coredef.BHT_SIZE)

  val TAG_WIDTH = coredef.VADDR_WIDTH - OFFSET_WIDTH - INDEX_WIDTH

  val tag = UInt(TAG_WIDTH.W)
  val valid = Bool()

  val history = UInt(coredef.BHT_WIDTH.W)

  def taken(tag: UInt) = {
    val ret = Wire(new BPUResult())
    ret.valid := valid && this.tag === tag
    ret.history := history
    ret
  }
}

class BPUResult(implicit val coredef: CoreDef) extends Bundle {
  val OFFSET_WIDTH = log2Ceil(coredef.L1I.TRANSFER_SIZE / 8)
  val INDEX_WIDTH = log2Ceil(coredef.BHT_SIZE)
  val TAG_WIDTH = coredef.VADDR_WIDTH - OFFSET_WIDTH - INDEX_WIDTH

  val valid = Bool()
  val history = UInt(coredef.BHT_WIDTH.W)

  def prediction = Mux(
    valid,
    Mux(history(coredef.BHT_WIDTH-1), BHTPrediction.taken, BHTPrediction.notTaken),
    BHTPrediction.missed
  )

  def up(tag: UInt): BHTSlot = {
    val ret = Wire(new BHTSlot())

    ret.valid := true.B
    ret.tag := tag

    when(this.valid) {
      ret.history := Mux(this.history.andR(), (-1).S(coredef.BHT_WIDTH.W).asUInt, this.history + 1.U)
    }.otherwise {
      ret.history := 1.U(1.W) ## 0.U((coredef.BHT_WIDTH - 1).W)
    }

    ret
  }

  def down(tag: UInt): BHTSlot = {
    val ret = Wire(new BHTSlot())

    ret.valid := true.B
    ret.tag := tag

    when(this.valid) {
      ret.history := Mux(this.history.orR(), this.history - 1.U, 0.U)
    }.otherwise {
      ret.history := 0.U(1.W) ## (-1).S((coredef.BHT_WIDTH - 1).W).asUInt
    }

    ret
  }

  def update(taken: Bool, tag: UInt) = Mux(taken, this.up(tag), this.down(tag))
}

class BPU(implicit val coredef: CoreDef) extends MultiIOModule {
  val toFetch = IO(new Bundle{
    val pc = Input(UInt(coredef.VADDR_WIDTH.W)) // the address (pc) of the query branch
    val results = Output(Vec(coredef.L1I.TRANSFER_SIZE / Const.INSTR_MIN_WIDTH, new BPUResult))
  })

  val toExec = IO(new Bundle{
    val valid = Input(Bool())
    val lpc = Input(UInt(coredef.XLEN.W)) // Only register on the last slot
    val hist = Input(new BPUResult)
    val taken = Input(Bool())
  })

  val INLINE_COUNT = coredef.L1I.TRANSFER_SIZE / Const.INSTR_MIN_WIDTH
  val OFFSET_WIDTH = log2Ceil(coredef.L1I.TRANSFER_SIZE / 8)
  val INDEX_WIDTH = log2Ceil(coredef.BHT_SIZE)
  val INDEX_OFFSET_WIDTH = OFFSET_WIDTH + INDEX_WIDTH
  val TAG_WIDTH = coredef.VADDR_WIDTH - OFFSET_WIDTH - INDEX_WIDTH

  def getIndex(addr: UInt) = addr(INDEX_OFFSET_WIDTH-1, OFFSET_WIDTH)
  def getTag(addr: UInt) = addr(coredef.VADDR_WIDTH-1, INDEX_OFFSET_WIDTH)
  def getOffset(addr: UInt) = addr(OFFSET_WIDTH-1, log2Ceil(Const.INSTR_MIN_WIDTH/8))
  def toAligned(addr: UInt) = getTag(addr) ## getIndex(addr) ## 0.U(OFFSET_WIDTH.W) // The input address should be aligned anyway

  val store = Mem(coredef.BHT_SIZE, Vec(INLINE_COUNT, new BHTSlot))

  val reseting = RegInit(true.B)
  val resetCnt = RegInit(0.U(log2Ceil(coredef.BHT_SIZE).W))

  // Prediction part
  val readout = store.read(getIndex(toFetch.pc))
  val tag = getTag(toFetch.pc)
  val pipeReadout = RegNext(readout)
  val pipeTag = RegNext(tag)
  toFetch.results := VecInit(pipeReadout.map(_.taken(pipeTag)))

  // Update part
  val updateTag = getTag(toExec.lpc)
  val updateOffset = getOffset(toExec.lpc)
  assert(updateOffset.getWidth == log2Ceil(coredef.L1I.TRANSFER_SIZE / Const.INSTR_MIN_WIDTH))
  val updated = toExec.hist.update(toExec.taken, updateTag)
  val updateMask = UIntToOH(updateOffset).asBools()

  val we = updateMask.map(bit => {
    val ret = WireDefault(bit)
    when(!toExec.valid) {
      ret := false.B
    }

    when(reseting) {
      ret := true.B
    }

    ret
  })

  val init = Wire(new BHTSlot)
  init := DontCare
  init.valid := false.B

  val waddr = Mux(reseting, resetCnt, getIndex(toExec.lpc))
  val data = VecInit(
    Seq.fill(coredef.L1I.TRANSFER_SIZE / Const.INSTR_MIN_WIDTH)(Mux(reseting, init, updated))
  )

  store.write(waddr, data, we)

  when(reseting) {
    resetCnt := resetCnt +% 1.U
    when(resetCnt.andR()) {
      reseting := false.B
    }
  }
}
