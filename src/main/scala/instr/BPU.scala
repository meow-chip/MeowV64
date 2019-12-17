package instr

import chisel3._
import _root_.cache._
import _root_.core._
import _root_.data._
import chisel3.util.log2Ceil
import chisel3.experimental.ChiselEnum

class BPTag(coredef: CoreDef, SIZE: Int) extends Bundle {
  val INDEX_OFFSET_WIDTH = log2Ceil(SIZE)
  val TAG_WIDTH = coredef.ADDR_WIDTH - INDEX_OFFSET_WIDTH

  val tag = UInt(TAG_WIDTH.W)
  val valid = Bool()
}

object BPTag {
  def default(coredef: CoreDef, SIZE: Int): ILine = {
    val ret = Wire(new ILine(opts))
    ret.tag := DontCare
    ret.valid := false.B

    ret
  }
}

object BPEntry extends ChiselEnum {
  val t, wt, wnt, nt = Value
}

class BPU(coredef: CoreDef, SIZE: Int, ASSOC: Int) extends MultiIOModule {
  val toFetch = IO(new Bundle{
                     val pc = Input(UInt(coredef.XLEN.W))
                     val query = Input(Bool())
                     val taken = Output(Bool())
                   })
  val toCtrl = IO(new Bundle{
                    val upd = Input(Bool())
                    val actpc = Input(UInt(coredef.XLEN.W))
                    val fired = Input(Bool())
                  })

  val LINE_PER_ASSOC = SIZE / ASSOC

  val ASSOC_IDX_WIDTH = log2Ceil(ASSOC)
  val OFFSET_WIDTH = 1 // Compressed instruction is 2 byte
  val INDEX_OFFSET_WIDTH = log2Ceil(SIZE / ASSOC)
  val INDEX_WIDTH = INDEX_OFFSET_WIDTH - OFFSET_WIDTH

  val directories = Mem(LINE_PER_ASSOC, Vec(ASSOC, new BPTag(coredef, SIZE)))
  val stores = SyncReadMem(LINE_PAER_ASSOC, Vec(ASSOC, new BPEntry()))

  val writerAddr = Wire(UINT(INDEX_WIDTH.W))
  val writerDir = Wire(new BPTag())
  val writerData = Wire(new BPEntry())
  val writerMask = Wire(Vec(ASSOC, Bool()))

  directories.write(writerAddr, VecInit(Seq.fill(opts.ASSOC)(writerDir)), writerMask)
  stores.write(writerAddr, VecInit(Seq.fill(opts.ASSOC)(writerData)), writerMask)

  writerAddr := DontCare
  writerDir := DontCare
  writerData := DontCare
  writerMask := VecInit(Seq.fill(opts.ASSOC)(false.B))

  assume(INDEX_WIDTH == log2Ceil(LINE_PER_ASSOC))

  val getIndex(addr: UInt) = addr(INDEX_OFFSET_WIDTH-1, OFFSET_WIDTH)
  val getTag(addr: UInt) = addr(coredef.ADDR_WIDTH-1, INDEX_OFFSET_WIDTH)
  def toAligned(addr: UInt) = getTag(addr) ## getIndex(addr) ## 0.U(1.W) // The input address should be aligned anyway

  // Prediction part
  val readouts = directories.read(getIndex(toFetch.pc))
  val dataReadouts = stores.read(getIndex(toFetch.pc))
  val hitMap = VecInit(readouts.map(r => r.valid && r.tag === getTag(toFetch.pc)))
  val pipeHitMap = RegNext(hitMap)
  val pipeQuery = RegNext(toFetch.query)
  val pipePC = RegNext(toFetch.pc)

  val rdata = Mux1H(dataReadouts.zipWithIndex.map({ case (line, idx) => pipeHitMap(idx) -> line }))

  when(pipeQuery) {
    when(pipeHitMap.asUInt().orR {
            toFetch.taken := (rdata === BPEntry.t) || (rdata === BPEntry.wt)
         }).otherwise {
            val victim = rand(ASSOC_IDX_WIDTH-1, 0)
            val mask = (0 until ASSOC).map(_.U === victim)
            val written = Wire(new BPTag())
            written.tag := getTag(pipePC)
            written.valid := true.B
            writerAddr := getIndex(pipePC)
            writerDir := written
            writerMask := mask
            writerData := BPEntry.wt

            toFetch.taken := false.B
    }
  }

  // Update part
  val updReads = directories.read(getIndex(toCtrl.actpc))
  val updDataReads = stores.read(getIndex(toCtrl.actpc))
  val updHitMap = VecInit(updReads.map(r => r.valid && r.tag === getTag(toCtrl.actpc)))
  val updRdata = Mux1H(dataReadouts.zipWithIndex.map({ case (line, idx) => pipeHitMap(idx) -> line }))
  val pipeUpdHit = RegNext(updHitMap)
  val pipeUpd = RegNext(toCtrl.upd)
  val pipeUpdPC = RegNext(toCtrl.actpc)
  val pipeFired = RegNext(toCtrl.fired)

  when(pipeUpd) {
    val victim = rand(ASSOC_IDX_WIDTH-1, 0)
    val mask = (0 until ASSOC).map(_.U === victim)
    val written = Wire(new BPTag())
    written.tag := getTag(pipeUpdPC)
    written.valid := true.B
    writerAddr := getIndex(pipeUpdPC)
    writerDir := written
    writerMask := mask

    when(pipeFired) {
      switch(updRdata) {
        is(BPEntry.t) {
          writerData := BPEntry.t
        }
        is(BPEntry.wt) {
          writerData := BPEntry.t
        }
        is(BPEntry.wnt) {
          writerData := BPEntry.wt
        }
        is(BPEntry.nt) {
          writerData := BPEntry.wnt
        }
      }
    }.otherwise {
      switch(updRdata) {
        is(BPEntry.t) {
          writerData := BPEntry.wt
        }
        is(BPEntry.wt) {
          writerData := BPEntry.wnt
        }
        is(BPEntry.wnt) {
          writerData := BPEntry.nt
        }
        is(BPEntry.nt) {
          writerData := BPEntry.nt
        }
      }
    }
  }
}
