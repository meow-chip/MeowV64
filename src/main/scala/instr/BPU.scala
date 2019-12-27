package instr

import chisel3._
import _root_.cache._
import _root_.core._
import _root_.data._
import chisel3.util._
import chisel3.experimental.ChiselEnum

class BPTag(val ADDR_WIDTH: Int, val SIZE: Int) extends Bundle {
  val INDEX_OFFSET_WIDTH = log2Ceil(SIZE)
  val TAG_WIDTH = ADDR_WIDTH - INDEX_OFFSET_WIDTH

  val tag = UInt(TAG_WIDTH.W)
  val valid = Bool()
}

object BPTag {
  def empty(ADDR_WIDTH: Int, SIZE: Int): BPTag = {
    val ret = Wire(new BPTag(ADDR_WIDTH, SIZE))
    ret.tag := DontCare
    ret.valid := false.B

    ret
  }
}

class BPEntry(val BIT: Int) extends Bundle {
  val entry = UInt(BIT.W)

  def taken() = {
    val upd = Wire(new BPEntry(BIT))
    upd.entry := Mux(entry.andR, entry, entry +% 1.U)
    upd
  }

  def notaken() = {
    val upd = Wire(new BPEntry(BIT))
    upd.entry := Mux(entry === 0.U, 0.U, entry -% 1.U)
    upd
  }

  def predict() = entry(BIT - 1)
}

class BPU(val XLEN: Int, val ADDR_WIDTH: Int, val SIZE: Int, val FETCH_NUM: Int, val BIT: Int) extends MultiIOModule {
  val toFetch = IO(new Bundle{
                    val query = Input(Bool()) // when query is true, BPU returns the prediction
                    val pc = Input(UInt(XLEN.W)) // the address (pc) of the query branch
                    val taken = Output(Vec(FETCH_NUM, Bool())) // the query result: will the branch be taken
                   })
  val toCtrl = IO(new Bundle{
                    val upd = Input(Bool()) // when upd is true, BPU updates the branch history table
                    val updPC = Input(UInt(XLEN.W)) // pc of the branch which is gonna to update
                    val fired = Input(Bool()) // does the branch be taken
                  })

  val LINE_PER_ASSOC = SIZE

  val OFFSET_WIDTH = log2Ceil(FETCH_NUM * Const.INSTR_MIN_WIDTH/8) // Compressed instruction is 2 byte
  val INDEX_WIDTH = log2Ceil(SIZE)
  val INDEX_OFFSET_WIDTH = INDEX_WIDTH + OFFSET_WIDTH

  val directories = Mem(LINE_PER_ASSOC, new BPTag(ADDR_WIDTH, SIZE))
  val stores = SyncReadMem(LINE_PER_ASSOC, Vec(FETCH_NUM, new BPEntry(BIT)))

  val writerAddr = Wire(UInt(INDEX_WIDTH.W))
  val writerDir = Wire(new BPTag(ADDR_WIDTH, SIZE))
  val writerData = Wire(new BPEntry(BIT))
  val writerMask = Wire(Vec(FETCH_NUM, Bool()))
  val writerTagEn = Wire(Bool())

  when(writerTagEn) {
    directories.write(writerAddr, writerDir)
  }
  stores.write(writerAddr, VecInit(Seq.fill(FETCH_NUM)(writerData)), writerMask)

  writerAddr := DontCare
  writerDir := DontCare
  writerData := DontCare
  writerMask := VecInit(Seq.fill(FETCH_NUM)(false.B))
  writerTagEn := false.B

  assume(INDEX_WIDTH == log2Ceil(LINE_PER_ASSOC))

  def getIndex(addr: UInt) = addr(INDEX_OFFSET_WIDTH-1, OFFSET_WIDTH)
  def getTag(addr: UInt) = addr(ADDR_WIDTH-1, INDEX_OFFSET_WIDTH)
  def getOffset(addr: UInt) = addr(OFFSET_WIDTH-1, log2Ceil(Const.INSTR_MIN_WIDTH/8))
  def toAligned(addr: UInt) = getTag(addr) ## getIndex(addr) ## 0.U(OFFSET_WIDTH.W) // The input address should be aligned anyway

  // Prediction part
  val readout = directories.read(getIndex(toFetch.pc))
  val dataReadouts = stores.read(getIndex(toFetch.pc))
  val hit = readout.valid && readout.tag === getTag(toFetch.pc)
  val pipeHit = RegNext(hit)
  val pipeQuery = RegNext(toFetch.query)
  val pipePC = RegNext(toFetch.pc)
  toFetch.taken := VecInit(Seq.fill(FETCH_NUM)(false.B))

  when(pipeQuery) {
    when(pipeHit) {
      toFetch.taken := VecInit(dataReadouts.map(_.predict()).toList)
    }.otherwise {
      val writtenTag = Wire(new BPTag(ADDR_WIDTH, SIZE))
      writtenTag.tag := getTag(pipePC)
      writtenTag.valid := true.B

      val mask = VecInit(Seq.fill(FETCH_NUM)(true.B))
      writerAddr := getIndex(pipePC)
      writerDir := writtenTag
      writerMask := mask
      val emptyEntry = Wire(new BPEntry(BIT))
      emptyEntry.entry := 1.U ## 0.U((BIT-1).W)
      writerData := emptyEntry
    }
  }

  // Update part
  val updAlignedPC = toAligned(toCtrl.updPC)
  val updReadout = directories.read(updAlignedPC)
  val updDataReadouts = stores.read(updAlignedPC)
  val updHit = updReadout.valid && updReadout.tag === getTag(updAlignedPC)
  val pipeUpd = RegNext(toCtrl.upd)
  val pipeUpdHit = RegNext(updHit)
  val pipeUpdPC = RegNext(updAlignedPC)
  val pipeFired = RegNext(toCtrl.fired)
  val pipeOffset = RegNext(getOffset(toCtrl.updPC))

  when(pipeUpd) {
    when(pipeUpdHit) {
      val mask = (0 until FETCH_NUM).map(_.U === pipeOffset)
      writerAddr := getIndex(pipeUpdPC)
      writerMask := mask
      when(pipeFired) {
        writerData := updDataReadouts(pipeOffset).taken()
      }.otherwise {
        writerData := updDataReadouts(pipeOffset).notaken()
      }
    }.otherwise {
      val writtenTag = Wire(new BPTag(ADDR_WIDTH, SIZE))
      writtenTag.tag := getTag(pipePC)
      writtenTag.valid := true.B
      writerAddr := getIndex(pipePC)
      writerDir := writtenTag
      writerTagEn := true.B

      val mask = VecInit(Seq.fill(FETCH_NUM)(true.B))
      writerMask := mask
      val emptyEntry = Wire(new BPEntry(BIT))
      emptyEntry.entry := 1.U(BIT.W)
      writerData := emptyEntry
      toFetch.taken := VecInit(Seq.fill(FETCH_NUM)(false.B))
    }
  }
}
