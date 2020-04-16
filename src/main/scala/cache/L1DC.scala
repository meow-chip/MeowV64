package cache

import chisel3._
import _root_.data._
import chisel3.util.log2Ceil
import chisel3.experimental.ChiselEnum
import chisel3.util._
import Chisel.experimental.chiselName
import cache.L1DCPort.L2Req
import cache.L1DCPort.L1Req
import paging.PTWExt
import _root_.core.CoreDef

class DCReader(val opts: L1DOpts) extends Bundle {
  val addr = Output(UInt(opts.ADDR_WIDTH.W))
  val read = Output(Bool())
  val data = Input(UInt(opts.TRANSFER_SIZE.W))
  val stall = Input(Bool())
}

class DCWriter(val opts: L1DOpts) extends Bundle {
  val addr = Output(UInt(opts.ADDR_WIDTH.W))
  val write = Output(Bool())
  val data = Output(UInt(opts.TRANSFER_SIZE.W))
  val be = Output(UInt((opts.TRANSFER_SIZE / 8).W))
  val stall = Input(Bool())
}

class DCFenceStatus(val opts: L1DOpts) extends Bundle {
  val wbufClear = Input(Bool())
}

class DLine(val opts: L1Opts) extends Bundle {
  val INDEX_OFFSET_WIDTH = log2Ceil(opts.SIZE / opts.ASSOC)
  val TAG_WIDTH = opts.ADDR_WIDTH - INDEX_OFFSET_WIDTH
  val TRANSFER_COUNT = opts.LINE_WIDTH * 8 / opts.TRANSFER_SIZE

  val tag = UInt(TAG_WIDTH.W)
  val valid = Bool()
  val dirty = Bool()
  val data = Vec(TRANSFER_COUNT, UInt(opts.TRANSFER_SIZE.W))
}

object DLine {
  def empty(opts: L1DOpts): DLine = {
    val ret = Wire(new DLine(opts))

    ret := DontCare
    ret.valid := false.B

    ret
  }
}

@chiselName
class L1DC(val opts: L1DOpts)(implicit coredef: CoreDef) extends MultiIOModule {
  // Constants and helpers
  val IGNORED_WIDTH = log2Ceil(opts.TRANSFER_SIZE / 8)
  val OFFSET_WIDTH = log2Ceil(opts.LINE_WIDTH)
  val LINE_PER_ASSOC = opts.SIZE / opts.ASSOC / opts.LINE_WIDTH
  val INDEX_WIDTH = log2Ceil(LINE_PER_ASSOC)
  val ASSOC_IDX_WIDTH = log2Ceil(opts.ASSOC)

  def getLineAddr(addr: UInt) = addr(opts.ADDR_WIDTH-1, IGNORED_WIDTH) ## 0.U(IGNORED_WIDTH.W)
  def getTag(addr: UInt) = addr >> (INDEX_WIDTH + OFFSET_WIDTH)
  def getIndex(addr: UInt) = addr(INDEX_WIDTH + OFFSET_WIDTH-1, OFFSET_WIDTH)
  def getSublineIdx(addr: UInt) = addr(OFFSET_WIDTH-1, IGNORED_WIDTH)

  def muxBE(be: UInt, wdata: UInt, base: UInt): UInt = {
    assume(be.getWidth * 8 == wdata.getWidth)
    assume(be.getWidth * 8 == base.getWidth)

    val ret = Wire(Vec(be.getWidth, UInt(8.W)))

    for((sel, (w, b), r) <- (be.asBools(), wdata.asTypeOf(ret).zip(base.asTypeOf(ret)), ret).zipped) {
      r := Mux(sel, w, b)
    }

    ret.asUInt
  }

  val stores = SyncReadMem(LINE_PER_ASSOC, Vec(opts.ASSOC, new DLine(opts)))

  // Ports, mr = Memory read, ptw = Page table walker
  val mr = IO(Flipped(new DCReader(opts)))
  val ptw = IO(Flipped(new PTWExt))
  val w = IO(Flipped(new DCWriter(opts)))
  val fs = IO(Flipped(new DCFenceStatus(opts)))
  val toL2 = IO(new L1DCPort(opts))

  // Convert mr + ptw to r
  val rarbiter = Module(new RRArbiter(UInt(coredef.PADDR_WIDTH.W), 2))
  val r = Wire(new DCReader(opts))
  r.read := rarbiter.io.out.valid
  rarbiter.io.out.ready := !r.stall
  r.addr := rarbiter.io.out.bits

  rarbiter.io.in(0) <> ptw.req
  rarbiter.io.in(1).valid := mr.read
  rarbiter.io.in(1).bits := mr.addr

  val current = Reg(UInt())
  when(rarbiter.io.out.fire()) {
    current := rarbiter.io.chosen
  }

  // Asserting that in-line offset is 0
  assert((!r.read) || r.addr(IGNORED_WIDTH-1, 0) === 0.U)
  assert((!w.write) || w.addr(IGNORED_WIDTH-1, 0) === 0.U)

  toL2.wdata := DontCare
  toL2.l1addr := DontCare
  toL2.l1req := L1Req.idle

  // Read pipes
  val pipeRead = RegInit(false.B)
  val pipeAddr = RegInit(0.U(opts.ADDR_WIDTH.W))

  // TODO: convert MR into ValidIO
  mr.stall := !rarbiter.io.in(1).ready
  mr.data := r.data
  ptw.resp.bits := r.data
  ptw.resp.valid := pipeRead && !r.stall && current === 0.U

  val queryAddr = Wire(UInt(opts.ADDR_WIDTH.W))
  val lookups = stores.read(getIndex(queryAddr))

  // Write FIFO
  class WriteEv(val opts: L1DOpts) extends Bundle {
    val addr = UInt(opts.ADDR_WIDTH.W)
    val be = UInt((opts.TRANSFER_SIZE / 8).W)
    val wdata = UInt(opts.TRANSFER_SIZE.W)
    val valid = Bool()
  }

  object WriteEv {
    def default(opts: L1DOpts): WriteEv = {
      val ret = Wire(new WriteEv(opts))

      ret.addr := DontCare
      ret.be := DontCare
      ret.wdata := DontCare
      ret.valid := false.B

      ret
    }
  }

  val wbuf = RegInit(VecInit(Seq.fill(opts.WRITE_BUF_DEPTH)(WriteEv.default(opts))))
  val WBUF_IDX_WIDTH = log2Ceil(opts.WRITE_BUF_DEPTH)
  val wbufHead = RegInit(0.U(WBUF_IDX_WIDTH.W))
  val wbufTail = RegInit(0.U(WBUF_IDX_WIDTH.W))

  fs.wbufClear := wbufHead === wbufTail

  val pendingRead = Wire(Bool())

  // Write handler

  object MainState extends ChiselEnum {
    val writing, reading, walloc, readingRefill, wallocRefill, readingSpin, idle, rst = Value
    // readingSpin is for making sure that reading has finished.
    // Maybe can be optimized (handshake w <-> r)
  }

  val state = RegInit(MainState.rst)
  val nstate = Wire(MainState())

  nstate := state
  state := nstate

  val l2data = RegInit(0.U(opts.LINE_WIDTH))

  val waddr = wbuf(wbufHead).addr
  val nwaddr = wbuf(wbufHead +% 1.U).addr
  assert(waddr(IGNORED_WIDTH-1, 0) === 0.U)

  val wlookupAddr = Wire(waddr.cloneType)
  when(state === MainState.idle) {
    wlookupAddr := MuxLookup(
      nstate.asUInt,
      waddr,
      Seq(
        (MainState.reading.asUInt(), pipeAddr),
        (MainState.readingRefill.asUInt(), pipeAddr),
        (MainState.readingSpin.asUInt(), pipeAddr)
      )
    )
  }.otherwise {
    wlookupAddr := MuxLookup(
      state.asUInt(),
      waddr,
      Seq(
        (MainState.reading.asUInt(), pipeAddr),
        (MainState.readingRefill.asUInt(), pipeAddr),
        (MainState.readingSpin.asUInt(), pipeAddr)
      )
    )
  }

  val wlookups = stores.read(getIndex(wlookupAddr))
  val whits = wlookups.map(line => line.valid && line.tag === getTag(waddr))
  val whit = VecInit(whits).asUInt.orR
  val wdirtyHits = wlookups.map(line => line.valid && line.tag === getTag(waddr) && line.dirty)
  val wdirtyHit = VecInit(wdirtyHits).asUInt.orR

  val rand = chisel3.util.random.LFSR(8)
  val victim = RegInit(0.U(ASSOC_IDX_WIDTH.W))

  // writing / reading is never directly gone to
  assert((nstate =/= MainState.writing && nstate =/= MainState.reading) || state === MainState.idle || nstate === state)

  // Write port
  val writing = Wire(Vec(opts.ASSOC, Bool()))
  val l1writing = Wire(Vec(opts.ASSOC, Bool()))
  val writingData = Wire(new DLine(opts))
  val writingAddr = Wire(UInt(INDEX_WIDTH.W))
  l1writing := VecInit(Seq.fill(opts.ASSOC)(false.B))
  writing := l1writing
  writingData := DontCare
  writingAddr := DontCare

  stores.write(writingAddr, VecInit(Seq.fill(opts.ASSOC)(writingData)), writing)

  // Rst
  val rstCnt = RegInit(0.U(log2Ceil(LINE_PER_ASSOC).W))

  val pipeAddrIdx = Wire(UInt())
  val waddrIdx = Wire(UInt())
  val wlookupIdx = Wire(UInt())
  pipeAddrIdx := getIndex(pipeAddr)
  waddrIdx := getIndex(waddr)
  wlookupIdx := getIndex(wlookupAddr)

  switch(state) {
    is(MainState.rst) {
      l1writing := VecInit(Seq.fill(opts.ASSOC)(true.B))
      writingAddr := rstCnt
      writingData := DLine.empty(opts)

      rstCnt := rstCnt +% 1.U

      when(rstCnt === (LINE_PER_ASSOC-1).U) {
        nstate := MainState.idle
      }
    }

    is(MainState.idle) {
      // Wait one extra cycle after walloc
      // for the allocated data to propagate to the read port
      when(pendingRead) {
        nstate := MainState.reading
      }.elsewhen(wbufHead =/= wbufTail) {
        nstate := MainState.writing
      }
    }

    is(MainState.writing) {
      toL2.l1addr := getTag(waddr) ## getIndex(waddr) ## 0.U(OFFSET_WIDTH.W)

      assert(wbufHead =/= wbufTail)

      def commit() {
        val hitMask = VecInit(wlookups.map(lookup => lookup.valid && lookup.tag === getTag(waddr)))
        val lookup = MuxCase(DLine.empty(opts), wlookups.zipWithIndex.map({ case (lookup, idx) => (
          hitMask(idx),
          lookup
        )}))

        val written = Wire(lookup.cloneType)
        written := lookup
        written.data(getSublineIdx(waddr)) := muxBE(
          wbuf(wbufHead).be,
          wbuf(wbufHead).wdata,
          lookup.data(getSublineIdx(waddr))
        )

        written.dirty := true.B

        l1writing := hitMask
        writingAddr := getIndex(waddr)
        writingData := written

        wbuf(wbufHead).valid := false.B
        wbufHead := wbufHead +% 1.U

        nstate := MainState.idle
      }

      when(
        RegNext(toL2.l2req) =/= L2Req.idle && RegNext(getIndex(toL2.l2addr)) === getIndex(waddr)
      ) {
        // Wait for one extra cycle
        nstate := MainState.writing
      }.elsewhen(wdirtyHit) {
        // Commit directly
        commit()
      }.elsewhen(whit) {
        // Is an write hit, send modify directly
        toL2.l1req := L1DCPort.L1Req.modify

        when(!toL2.l1stall) {
          // Commit
          commit()
        }
      }.otherwise {
        // Not hit, goto walloc
        nstate := MainState.walloc
        victim := rand(ASSOC_IDX_WIDTH-1, 0)
      }
    }

    is(MainState.reading) {
      when(!wlookups(victim).valid || !wlookups(victim).dirty) {
        nstate := MainState.readingRefill
      }.otherwise {
        toL2.l1addr := wlookups(victim).tag ## getIndex(pipeAddr) ## 0.U(OFFSET_WIDTH.W)
        toL2.l1req := L1DCPort.L1Req.writeback
        toL2.wdata := wlookups(victim).data.asUInt()

        val invalid = Wire(new DLine(opts))
        invalid := DontCare
        invalid.valid := false.B

        when(!toL2.l1stall) {
          // Must be an read-miss
          l1writing(victim) := true.B
          writingAddr := getIndex(pipeAddr)
          writingData := invalid

          nstate := MainState.readingRefill
        }
      }
    }

    is(MainState.walloc) {
      when(!wlookups(victim).valid || !wlookups(victim).dirty) {
        nstate := MainState.wallocRefill
      }.otherwise {
        toL2.l1addr := wlookups(victim).tag ## getIndex(waddr) ## 0.U(OFFSET_WIDTH.W)
        toL2.l1req := L1DCPort.L1Req.writeback
        toL2.wdata := wlookups(victim).data.asUInt()

        val invalid = Wire(new DLine(opts))
        invalid := DontCare
        invalid.valid := false.B

        when(!toL2.l1stall) {
          // Must be an read-miss
          l1writing(victim) := true.B
          writingAddr := getIndex(waddr)
          writingData := invalid

          nstate := MainState.wallocRefill
        }
      }
    }

    is(MainState.wallocRefill, MainState.readingRefill) {
      toL2.l1req := Mux(state === MainState.readingRefill, L1DCPort.L1Req.read, L1DCPort.L1Req.modify)
      val l1addr = Mux(state === MainState.readingRefill, pipeAddr, waddr)
      toL2.l1addr := getTag(l1addr) ## getIndex(l1addr) ## 0.U(OFFSET_WIDTH.W)

      val writtenAddr = Mux(state === MainState.readingRefill, pipeAddr, waddr)

      val written = Wire(new DLine(opts))
      written.valid := true.B
      written.dirty := state === MainState.wallocRefill
      written.tag := getTag(writtenAddr)
      written.data := toL2.rdata.asTypeOf(written.data)

      when(state === MainState.wallocRefill) {
        written.data(getSublineIdx(waddr)) := muxBE(
          wbuf(wbufHead).be,
          wbuf(wbufHead).wdata,
          toL2.rdata.asTypeOf(written.data)(getSublineIdx(waddr))
        )
      }

      when(!toL2.l1stall) {
        l1writing(victim) := true.B
        writingAddr := getIndex(writtenAddr)
        writingData := written

        when(state === MainState.readingRefill) {
          nstate := MainState.readingSpin
        }.otherwise {
          wbuf(wbufHead).valid := false.B
          wbufHead := wbufHead +% 1.U

          nstate := MainState.idle
        }
      }
    }

    // TODO: drop spin stage
    is(MainState.readingSpin) {
      when(!pendingRead) {
        nstate := MainState.idle
      }
    }
  }

  // Handle write interface
  // This operates synchronizely
  val wmHits = wbuf.map(ev => ev.valid && ev.addr === w.addr)
  val wmHit = VecInit(wmHits).asUInt().orR
  val wmHitHead = wbuf(wbufHead).valid && wbuf(wbufHead).addr === w.addr

  when(!w.write) {
    // No request
    w.stall := false.B
  }.elsewhen(wmHitHead) {
    // Head may be being processed right now, wait for it to finish and do a push
    w.stall := true.B
  }.otherwise {
    for(buf <- wbuf) {
      when(buf.addr === w.addr) {
        buf.wdata := muxBE(w.be, w.data, buf.wdata)
        buf.be := buf.be | w.be
      }
    }

    when(wmHit) {
      // Write merge completing
      w.stall := false.B
    }.elsewhen(wbufTail +% 1.U =/= wbufHead) {
      // Write merge miss, waiting to push
      assert(waddr(IGNORED_WIDTH-1, 0) === 0.U)
      wbuf(wbufTail).addr := w.addr
      wbuf(wbufTail).be := w.be
      wbuf(wbufTail).wdata := w.data
      wbuf(wbufTail).valid := true.B

      wbufTail := wbufTail +% 1.U

      w.stall := false.B
    }.otherwise {
      // Wtire merge miss, fifo full, wait for fifo
      w.stall := true.B
    }
  }

  // Handle read interface
  // For debug use only
  val hitCount = PopCount(lookups.map(line => line.valid && line.tag === getTag(pipeAddr)))
  assert(hitCount <= 1.U)

  val hits = lookups.map(line => line.valid && line.tag === getTag(pipeAddr))
  val hit = VecInit(hits).asUInt().orR
  val lookupRdata = Mux1H(lookups.map(line => (
    line.valid && line.tag === getTag(pipeAddr),
    line.data(getSublineIdx(pipeAddr))
  )))

  val pendingWHits= wbuf.map(buf => buf.valid && buf.addr === getLineAddr(pipeAddr))
  val pendingWdata = Mux1H(pendingWHits, wbuf.map(_.wdata))
  val pendingBe = Mux1H(pendingWHits, wbuf.map(_.be))

  val rdata = Mux(VecInit(pendingWHits).asUInt.orR, muxBE(pendingBe, pendingWdata, lookupRdata), lookupRdata)

  when(!r.stall) {
    pipeRead := r.read
    pipeAddr := r.addr

    queryAddr := r.addr
  }.otherwise {
    queryAddr := pipeAddr
  }

  pendingRead := false.B

  // FIXME: change this one back?
  when(RegNext(toL2.l2req) =/= L2Req.idle) {
    // read port occupied
    r.stall := true.B
  }.elsewhen((!pipeRead) || hit) {
    r.stall := false.B
  }.otherwise {
    r.stall := true.B
    pendingRead := true.B
  }

  r.data := rdata

  // L2 Handler
  toL2.l2stall := false.B
  val lookupReady = RegNext(toL2.l2stall)

  val l2Rdata = MuxCase(0.U, lookups.map(line => (
    line.valid && line.tag === getTag(toL2.l2addr),
    line.data.asUInt()
  )))

  when(toL2.l2req =/= L2Req.idle) {
    // assert(toL2.l1stall) // L2req can only happen when l2 is processing other ports' requests
    // Nope that may happen. Suppose we are to victimize a line held by this L1

    queryAddr := toL2.l2addr
    toL2.l2stall := true.B

    when(lookupReady) { // To generate only two rw ports
      // For flushes, hit is asserted
      // For invals, wdata is ignored
      // So we should be safe to just use l2Rdata here without checking
      toL2.wdata := l2Rdata

      val written = Wire(new DLine(opts))
      written.data := l2Rdata.asTypeOf(written.data)
      written.tag := getTag(toL2.l2addr)
      written.dirty := false.B
      written.valid := toL2.l2req =/= L2Req.inval

      // TODO: assert hit for flush,
      // Assert !dirty for inval


      /**
       * Although the next read/write may not fetch the updated value,
       * this doesn't violate the memory model, because the memory operations
       * on the same core are still compatible with the program order. Only load/stores from another core
       * is affected, and writes from this core cannot propagate into other cores within one cycle.
       */
      when(!l1writing.asUInt().orR) {
        toL2.l2stall := false.B
        val hitmask = VecInit(lookups.map(lookup => lookup.valid && lookup.tag === getTag(toL2.l2addr)))
        writing := hitmask
        writingData := written
        writingAddr := getIndex(toL2.l2addr)
      }
    }
  }
}
