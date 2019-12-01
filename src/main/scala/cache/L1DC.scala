package cache

import chisel3._
import _root_.data._
import chisel3.util.log2Ceil
import chisel3.experimental.ChiselEnum
import chisel3.util._
import Chisel.experimental.chiselName
import cache.L1DCPort.L2Req
import cache.L1DCPort.L1Req

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

class DLine(val opts: L1Opts) extends Bundle {
  val INDEX_OFFSET_WIDTH = log2Ceil(opts.SIZE)
  val TAG_WIDTH = opts.ADDR_WIDTH - INDEX_OFFSET_WIDTH
  val TRANSFER_COUNT = opts.LINE_WIDTH * 8 / opts.TRANSFER_SIZE

  val tag = UInt(TAG_WIDTH.W)
  val valid = Bool()
  val dirty = Bool()
  val data = Vec(TRANSFER_COUNT, UInt(opts.TRANSFER_SIZE.W))
}

@chiselName
class L1DC(val opts: L1DOpts) extends MultiIOModule {
  // Constants and helpers
  val IGNORED_WIDTH = log2Ceil(opts.TRANSFER_SIZE / 8)
  val OFFSET_WIDTH = log2Ceil(opts.LINE_WIDTH)
  val LINE_PER_ASSOC = opts.SIZE / opts.ASSOC / opts.LINE_WIDTH
  val INDEX_WIDTH = log2Ceil(LINE_PER_ASSOC)
  val ASSOC_IDX_WIDTH = log2Ceil(opts.ASSOC)

  def getLineAddr(addr: UInt) = addr(opts.ADDR_WIDTH-1, OFFSET_WIDTH) ## 0.U(OFFSET_WIDTH.W)
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

  val stores = Seq.fill(opts.ASSOC)({ SyncReadMem(LINE_PER_ASSOC, new DLine(opts)) })

  // Ports
  val r = IO(Flipped(new DCReader(opts)))
  val w = IO(Flipped(new DCWriter(opts)))
  val toL2 = IO(new L1DCPort(opts))

  assert(r.addr(IGNORED_WIDTH-1, 0) === 0.U)
  assert(w.addr(IGNORED_WIDTH-1, 0) === 0.U)

  toL2.wdata := DontCare
  toL2.l1addr := DontCare
  toL2.l1req := L1Req.idle

  // Read pipes
  val pipeRead = RegInit(false.B)
  val pipeAddr = RegInit(0.U(opts.ADDR_WIDTH.W))

  val queryAddr = Wire(UInt(opts.ADDR_WIDTH.W))
  val lookups = VecInit(stores.map(_.read(getIndex(queryAddr))))

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

  val pendingRead = Wire(Bool())

  // Write handler

  object MainState extends ChiselEnum {
    // FIXME: reset!

    val writing, reading, walloc, readingRefill, wallocRefill, readingSpin, idle = Value
    // readingSpin is for making sure that reading has finished.
    // Maybe can be optimized (handshake w <-> r)
  }

  val state = RegInit(MainState.idle)
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

  val wlookups = VecInit(stores.map(_.read(getIndex(wlookupAddr), toL2.l2req === L2Req.idle)))
  val whit = wlookups.foldLeft(false.B)((acc, line) => acc || (line.valid && line.tag === getTag(waddr)))

  val rand = chisel3.util.random.LFSR(8)
  val victim = RegInit(0.U(ASSOC_IDX_WIDTH.W))

  // writing / reading is never directly gone to
  assert((nstate =/= MainState.writing && nstate =/= MainState.reading) || state === MainState.idle || nstate === state)

  switch(state) {
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

      when(whit) {
        // Is an write hit, send modify directly
        toL2.l1req := L1DCPort.L1Req.modify

        when(!toL2.l1stall) {
          // Commit
          for((lookup, idx) <- wlookups.zipWithIndex) {
            when(lookup.valid && lookup.tag === getTag(waddr)) {
              val written = Wire(lookup.cloneType)
              written := lookup

              written.data(getSublineIdx(waddr)) := muxBE(
                wbuf(wbufHead).be,
                wbuf(wbufHead).wdata,
                lookup.data(getSublineIdx(waddr))
              )

              stores(idx).write(getIndex(waddr), written)
            }
          }

          wbuf(wbufHead).valid := false.B
          wbufHead := wbufHead +% 1.U

          nstate := MainState.idle
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
          for((store, idx) <- stores.zipWithIndex) {
            when(idx.U === victim) {
              store.write(getIndex(pipeAddr), invalid)
            }
          }

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
          for((store, idx) <- stores.zipWithIndex) {
            when(idx.U === victim) {
              store.write(getIndex(waddr), invalid)
            }
          }

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
        for((store, idx) <- stores.zipWithIndex) {
          when(idx.U === victim) {
            store.write(getIndex(writtenAddr), written)
          }
        }

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
  // FIXME: write-merge
  when(!w.write) {
    w.stall := false.B
  }.elsewhen(wbufTail +% 1.U =/= wbufHead) {
    wbuf(wbufTail).addr := w.addr
    wbuf(wbufTail).be := w.be
    wbuf(wbufTail).wdata := w.data
    wbuf(wbufTail).valid := true.B

    wbufTail := wbufTail +% 1.U

    w.stall := false.B
  }.otherwise {
    w.stall := true.B
  }

  // Handle read interface
  // For debug use only
  val hitCount = PopCount(lookups.map(line => line.valid && line.tag === getTag(pipeAddr)))
  assert(hitCount <= 1.U)

  val hit = lookups.foldLeft(false.B)((acc, line) => acc || (line.valid && line.tag === getTag(pipeAddr)))
  val lookupRdata = MuxCase(0.U, lookups.map(line => (
    line.valid && line.tag === getTag(pipeAddr),
    line.data(getSublineIdx(pipeAddr))
  )))

  val pendingWdata = MuxCase(0.U, wbuf.map(buf => (
    buf.valid && buf.addr === getLineAddr(pipeAddr),
    buf.wdata
  )))
  val pendingBe = MuxCase(0.U, wbuf.map(buf => (
    buf.valid && buf.addr === getLineAddr(pipeAddr),
    buf.be
  )))

  val rdata = muxBE(pendingBe, pendingWdata, lookupRdata)

  when(!r.stall) {
    pipeRead := r.read
    pipeAddr := r.addr

    queryAddr := r.addr
  }.otherwise {
    queryAddr := pipeAddr
  }

  pendingRead := false.B

  when(toL2.l2req =/= L2Req.idle) {
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
    assert(toL2.l1stall) // L2req can only happen when l2 is processing other ports' requests

    queryAddr := toL2.l2addr
    toL2.l2stall := true.B

    when(lookupReady) { // To generate only two rw ports
      // For flushes, hit is asserted
      // For invals, wdata is ignored
      // So we should be safe to just use l2Rdata here without checking
      toL2.wdata := l2Rdata
      toL2.l2stall := false.B

      val written = Wire(new DLine(opts))
      written.data := l2Rdata.asTypeOf(written.data)
      written.tag := getTag(toL2.l2addr)
      written.dirty := false.B
      written.valid := toL2.l2req =/= L2Req.inval

      // TODO: assert hit for flush,
      // Assert !dirty for inval

      for((lookup, store) <- lookups.zip(stores)) {
        when(lookup.valid && lookup.tag === getTag(toL2.l2addr)) {

          /**
           * Although the next read/write may not fetch the updated value,
           * this doesn't violate the memory model, because the memory operations
           * on the same core are still compatible with the program order. Only load/stores from another core
           * is affected, and writes from this core cannot propagate into other cores within one cycle.
           */
          store.write(getIndex(toL2.l2addr), written)
        }
      }
    }
  }
}
