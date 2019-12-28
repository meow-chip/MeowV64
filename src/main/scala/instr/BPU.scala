package instr

import chisel3._
import _root_.cache._
import _root_.core._
import _root_.data._
import chisel3.util._
import chisel3.experimental.ChiselEnum

object BHTPerdiction extends ChiselEnum {
  val taken, notTaken, missed = Value
}

class BHTSlot(implicit val coredef: CoreDef) extends Bundle {
  val OFFSET_WIDTH = log2Ceil(Const.INSTR_MIN_WIDTH / 8 * coredef.FETCH_NUM)
  val INDEX_WIDTH = log2Ceil(coredef.BHT_SIZE)

  val TAG_WIDTH = coredef.ADDR_WIDTH - OFFSET_WIDTH - INDEX_WIDTH

  val tag = UInt(TAG_WIDTH.W)
  val valid = Bool()

  val history = UInt(coredef.BHT_WIDTH.W)

  def taken(tag: UInt) = {
    Mux(
      valid && this.tag === tag,
      Mux(history(coredef.BHT_WIDTH-1), BHTPerdiction.taken, BHTPerdiction.notTaken),
      BHTPerdiction.missed
    )
  }

  def up(tag: UInt): BHTSlot = {
    val ret = Wire(new BHTSlot())

    ret.valid := true.B
    ret.tag := tag

    when(this.valid && this.tag === tag) {
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

    when(this.valid && this.tag === tag) {
      ret.history := Mux(this.history.orR(), this.history - 1.U, 0.U)
    }.otherwise {
      ret.history := 0.U(1.W) ## (-1).S((coredef.BHT_WIDTH - 1).W).asUInt
    }

    ret
  }
}

class BPU(implicit val coredef: CoreDef) extends MultiIOModule {
  val toFetch = IO(new Bundle{
    val pc = Input(UInt(coredef.ADDR_WIDTH.W)) // the address (pc) of the query branch
    val taken = Output(Vec(coredef.FETCH_NUM, BHTPerdiction())) // the query result: will the branch be taken
  })

  val toExec = IO(new Bundle{
    val valid = Input(Bool()) // when valid is true, BPU updates the branch history table
    val pc = Input(UInt(coredef.XLEN.W)) // pc of the branch which is gonna to update
    val taken = Input(Bool()) // does the branch be taken
  })

  val OFFSET_WIDTH = log2Ceil(Const.INSTR_MIN_WIDTH / 8 * coredef.FETCH_NUM)
  val INDEX_WIDTH = log2Ceil(coredef.BHT_SIZE)
  val INDEX_OFFSET_WIDTH = OFFSET_WIDTH + INDEX_WIDTH
  val TAG_WIDTH = coredef.ADDR_WIDTH - OFFSET_WIDTH - INDEX_WIDTH

  def getIndex(addr: UInt) = addr(INDEX_OFFSET_WIDTH-1, OFFSET_WIDTH)
  def getTag(addr: UInt) = addr(coredef.ADDR_WIDTH-1, INDEX_OFFSET_WIDTH)
  def getOffset(addr: UInt) = addr(OFFSET_WIDTH-1, log2Ceil(Const.INSTR_MIN_WIDTH/8))
  def toAligned(addr: UInt) = getTag(addr) ## getIndex(addr) ## 0.U(OFFSET_WIDTH.W) // The input address should be aligned anyway

  val store = Mem(coredef.BHT_SIZE, Vec(coredef.ISSUE_NUM, new BHTSlot))

  val reseting = RegInit(true.B)
  val resetCnt = RegInit(0.U(log2Ceil(coredef.BHT_SIZE)))

  // Prediction part
  val readout = store.read(getIndex(toFetch.pc))
  val tag = getTag(toFetch.pc)
  val pipeReadout = RegNext(readout)
  val pipeTag = RegNext(tag)
  toFetch.taken := VecInit(pipeReadout.map(_.taken(pipeTag)))
  when(reseting) {
    toFetch.taken := VecInit(Seq.fill(coredef.ISSUE_NUM)(BHTPerdiction.missed))
  }

  // Update part
  val updateTag = getTag(toExec.pc)
  val updateOffset = getOffset(toExec.pc)
  assert(updateOffset.getWidth == log2Ceil(coredef.ISSUE_NUM))
  val updateReadout = store.read(getIndex(toFetch.pc))
  val updated = updateReadout.map(e => {
    Mux(toExec.taken, e.up(updateTag), e.down(updateTag))
  })
  val updateMask = VecInit(Seq.tabulate(coredef.ISSUE_NUM)(idx => idx.U === updateOffset))

  when(reseting) {
    store.write(resetCnt, VecInit(Seq.fill(coredef.ISSUE_NUM)({
      val init = Wire(new BHTSlot)
      init := DontCare
      init.valid := false.B

      init
    })))

    resetCnt := resetCnt +% 1.U
    when(resetCnt.andR()) {
      reseting := false.B
    }
  }.elsewhen(toExec.valid) {
    store.write(getIndex(toExec.pc), VecInit(updated), updateMask)
  }
}
