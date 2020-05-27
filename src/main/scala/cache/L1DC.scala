package cache

import chisel3._
import _root_.data._
import chisel3.util.log2Ceil
import chisel3.experimental.ChiselEnum
import chisel3.util._
import Chisel.experimental.chiselName
import cache.L1DCPort.L2Req
import cache.L1DCPort.L1Req
import _root_.core.CoreDef
import _root_.util.FlushableSlot

class DCRead(implicit val coredef: CoreDef) extends Bundle {
  val addr = UInt(coredef.PADDR_WIDTH.W)
  val reserve = Bool()
}

object DCRead {
  def load(addr: UInt)(implicit coredef: CoreDef): DCRead = {
    val ret = Wire(new DCRead)
    ret.reserve := false.B
    ret.addr := addr
    ret
  }

  def lr(addr: UInt)(implicit coredef: CoreDef) = {
    val ret = Wire(new DCRead)
    ret.reserve := true.B
    ret.addr := addr
  }
}

class DCReader(implicit val coredef: CoreDef) extends Bundle {
  val req = DecoupledIO(new DCRead)
  val resp = Input(ValidIO(UInt(coredef.XLEN.W)))
}

class DCInnerReader(val opts: L1DOpts) extends Bundle {
  val addr = Output(UInt(opts.ADDR_WIDTH.W))
  val reserve = Output(Bool())
  val read = Output(Bool())
  val data = Input(UInt(opts.TRANSFER_SIZE.W))
  val stall = Input(Bool())
}

object DCWriteOp extends ChiselEnum {
  val idle = Value
  val write = Value
  val cond = Value // Write cond
  val swap, add, and, or, xor, max, maxu, min, minu = Value // TODO: optimize LUT
  val commitLR = Value // Commit pending LR
}

object DCWriteLen extends ChiselEnum {
  val B, H, W, D = Value

  def toAXISize(len: DCWriteLen.Type) = Mux1H(
    Seq(
      (len === DCWriteLen.B) -> AXI.Constants.Size.S1.U,
      (len === DCWriteLen.H) -> AXI.Constants.Size.S2.U,
      (len === DCWriteLen.W) -> AXI.Constants.Size.S4.U,
      (len === DCWriteLen.D) -> AXI.Constants.Size.S8.U,
    )
  )
}

class DCWriter(val opts: L1DOpts) extends Bundle {
  val addr = Output(UInt(opts.ADDR_WIDTH.W)) // Offset is now embedded inside addr
  val len = Output(DCWriteLen())
  val op = Output(DCWriteOp())

  val wdata = Output(UInt(opts.TRANSFER_SIZE.W)) // WData should be sign extended

  val rdata = Input(UInt(opts.TRANSFER_SIZE.W)) // AMOSWAP and friends
  val stall = Input(Bool())

  // Raw byte enable
  def be = {
    val offset = addr(log2Ceil(opts.TRANSFER_SIZE/8)-1, 0)
    val mask = MuxLookup(len.asUInt(), 0.U, Seq(
      DCWriteLen.B.asUInt -> 0x1.U,
      DCWriteLen.H.asUInt -> 0x3.U,
      DCWriteLen.W.asUInt -> 0xF.U,
      DCWriteLen.D.asUInt -> 0xFF.U
    ))
    val sliced = Wire(UInt((opts.TRANSFER_SIZE / 8).W))
    sliced := mask << offset
    sliced
  }

  // Shifted wdata
  def sdata = {
    val offset = addr(log2Ceil(opts.TRANSFER_SIZE/8)-1, 0)
    val sliced = Wire(UInt(opts.TRANSFER_SIZE.W))
    sliced := wdata << (offset << 3)
    sliced
  }

  def aligned = addr(opts.ADDR_WIDTH-1, 3) ## 0.U(3.W)
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
  val mr = IO(Flipped(new DCReader))
  val ptw = IO(Flipped(new DCReader))
  val w = IO(Flipped(new DCWriter(opts)))
  val fs = IO(Flipped(new DCFenceStatus(opts)))
  val toL2 = IO(new L1DCPort(opts))

  // Convert mr + ptw to r
  val rArbiter = Module(new RRArbiter(new DCRead, 2))
  class RReq extends Bundle {
    val addr = UInt(coredef.PADDR_WIDTH.W)
    val chosen = UInt()
  }

  val r = Wire(new DCInnerReader(opts))
  r.read := rArbiter.io.out.valid
  rArbiter.io.out.ready := !r.stall
  r.addr := rArbiter.io.out.bits.addr
  r.reserve := rArbiter.io.out.bits.reserve

  rArbiter.io.in(0) <> ptw.req
  rArbiter.io.in(1) <> mr.req

  val current = Reg(UInt())
  when(rArbiter.io.out.fire()) {
    current := rArbiter.io.chosen
  }

  // Asserting that in-line offset is 0
  assert((!r.read) || r.addr(IGNORED_WIDTH-1, 0) === 0.U)
  // assert((!w.write) || w.addr(IGNORED_WIDTH-1, 0) === 0.U)
  //   The check for write is no longer true, because of AMO

  toL2.wdata := DontCare
  toL2.l1addr := DontCare
  toL2.l1req := L1Req.idle

  // Read pipes
  val pipeRead = RegInit(false.B)
  val pipeReserve = RegInit(false.B)
  val pipeAddr = RegInit(0.U(opts.ADDR_WIDTH.W))

  ptw.resp.bits := r.data
  ptw.resp.valid := pipeRead && !r.stall && current === 0.U
  mr.resp.bits := r.data
  mr.resp.valid := pipeRead && !r.stall && current === 1.U

  val queryAddr = Wire(UInt(opts.ADDR_WIDTH.W))
  val lookups = stores.read(getIndex(queryAddr))

  // AMO/SC stuff
  val amoalu = Module(new AMOALU(opts))
  amoalu.io := DontCare
  val resValid = RegInit(false.B)
  val resCommitted = RegInit(false.B)
  val reserved = Reg(UInt(opts.ADDR_WIDTH.W))
  def reserveMatch(addr: UInt) = (
    resValid && resCommitted
    && getTag(reserved) === getTag(addr)
    && getIndex(reserved) === getIndex(addr)
  )

  val pendingWOp = RegNext(w.op)
  val pendingWData = RegNext(w.wdata)
  val pendingWLen = RegNext(w.len)
  val pendingWAddr = RegNext(w.addr)
  amoalu.io.offset := pendingWAddr(log2Ceil(opts.XLEN/8)-1, 0)
  amoalu.io.wdata := pendingWData
  amoalu.io.length := pendingWLen
  amoalu.io.op := pendingWOp
  val pendingWret = RegInit(0.U(opts.XLEN.W))

  // Write FIFO
  class WriteEv(val opts: L1DOpts) extends Bundle {
    val aligned = UInt(opts.ADDR_WIDTH.W)
    val be = UInt((opts.TRANSFER_SIZE / 8).W)
    val sdata = UInt(opts.TRANSFER_SIZE.W)
    val isAMO = Bool()
    val isCond = Bool()
    val valid = Bool()
  }

  object WriteEv {
    def default(opts: L1DOpts): WriteEv = {
      val ret = Wire(new WriteEv(opts))

      ret.aligned := DontCare
      ret.be := DontCare
      ret.sdata := DontCare
      ret.valid := false.B
      ret.isAMO := false.B
      ret.isCond := false.B

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

  val waddr = wbuf(wbufHead).aligned
  val nwaddr = wbuf(wbufHead +% 1.U).aligned
  assert(wbufHead === wbufTail || waddr(IGNORED_WIDTH-1, 0) === 0.U)

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

  // FIXME: merge this with lookups
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
        victim := rand(ASSOC_IDX_WIDTH-1, 0)
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
          wbuf(wbufHead).sdata,
          lookup.data(getSublineIdx(waddr))
        )

        when(wbuf(wbufHead).isAMO) {
          amoalu.io.rdata := lookup.data(getSublineIdx(waddr))
          written.data(getSublineIdx(waddr)) := amoalu.io.muxed
          pendingWret := amoalu.io.rsliced
        }.otherwise {
          pendingWret := 0.U // Successful SC
        }

        when(wbuf(wbufHead).isCond) {
          resValid := false.B
        }

        written.dirty := true.B

        l1writing := hitMask
        writingAddr := getIndex(waddr)
        writingData := written

        wbuf(wbufHead).valid := false.B
        wbufHead := wbufHead +% 1.U

        nstate := MainState.idle
      }

      when(
        wbuf(wbufHead).isCond && !reserveMatch(waddr)
      ) {
        // SC failed
        pendingWret := 1.U
        wbuf(wbufHead).valid := false.B
        wbufHead := wbufHead +% 1.U
        nstate := MainState.idle
        resValid := false.B
      }.elsewhen(
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
      when(wlookups(victim).valid && wlookups(victim).tag === getTag(reserved)) {
        // Reselect victim
        victim := victim +% 1.U
      }.elsewhen(!wlookups(victim).valid || !wlookups(victim).dirty) {
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
          wbuf(wbufHead).sdata,
          toL2.rdata.asTypeOf(written.data)(getSublineIdx(waddr))
        )

        when(wbuf(wbufHead).isAMO) {
          amoalu.io.rdata := toL2.rdata.asTypeOf(written.data)(getSublineIdx(waddr))
          written.data(getSublineIdx(waddr)) := amoalu.io.muxed
          pendingWret := amoalu.io.rsliced
        }
      }

      when(!toL2.l1stall) {
        writingAddr := getIndex(writtenAddr)
        writingData := written

        when(state === MainState.readingRefill) {
          l1writing(victim) := true.B
          nstate := MainState.readingSpin
        }.otherwise {
          pendingWret := 0.U
          l1writing(victim) := true.B

          when(wbuf(wbufHead).isCond) {
            resValid := false.B
            when(!reserveMatch(waddr)) {
              // SC failed
              pendingWret := 1.U
              l1writing(victim) := false.B
            }
          }

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
  val wmHits = wbuf.map(ev => ev.valid && ev.aligned === w.aligned)
  val wmHit = VecInit(wmHits).asUInt().orR
  val wmHitHead = wbuf(wbufHead).valid && wbuf(wbufHead).aligned === w.aligned

  w.rdata := pendingWret
  val pushed = RegInit(false.B) // Pushed state for

  when(w.op === DCWriteOp.idle) {
    // No request
    w.stall := false.B
  }.elsewhen(w.op === DCWriteOp.commitLR) {
    w.stall := false.B
    when(getTag(w.addr) === getTag(reserved) && getIndex(w.addr) === getIndex(reserved)) {
      resCommitted := true.B
    }
  }.elsewhen(w.op =/= DCWriteOp.write) {
    // Wait for write queue to clear up
    when(!pushed) {
      w.stall := true.B
      when(wbufTail +% 1.U =/= wbufHead) {
        wbuf(wbufTail).aligned := w.aligned
        wbuf(wbufTail).be := w.be
        wbuf(wbufTail).sdata := w.sdata
        wbuf(wbufTail).valid := true.B
        wbuf(wbufTail).isAMO := w.op =/= DCWriteOp.cond
        wbuf(wbufTail).isCond := w.op === DCWriteOp.cond

        wbufTail := wbufTail +% 1.U

        pushed := true.B
      }
    }.elsewhen(wbufHead === wbufTail) {
      pushed := false.B
      w.stall := false.B
    }.otherwise {
      w.stall := true.B
    }
  }.elsewhen(wmHitHead) {
    // Head may be being processed right now, wait for it to finish and do a push
    w.stall := true.B
  }.otherwise {
    for(buf <- wbuf) {
      when(buf.aligned === w.aligned) {
        buf.sdata := muxBE(w.be, w.sdata, buf.sdata)
        buf.be := buf.be | w.be
      }
    }

    when(wmHit) {
      // Write merge completing
      w.stall := false.B
    }.elsewhen(wbufTail +% 1.U =/= wbufHead) {
      // Write merge miss, waiting to push
      assert(w.aligned(IGNORED_WIDTH-1, 0) === 0.U)
      wbuf(wbufTail).aligned := w.aligned
      wbuf(wbufTail).be := w.be
      wbuf(wbufTail).sdata := w.sdata
      wbuf(wbufTail).valid := true.B
      wbuf(wbufTail).isAMO := false.B
      wbuf(wbufTail).isCond := false.B

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
  val storeJustWrittenData = RegNext(writingData)
  val storeJustWritten = (
    RegNext(writing).asUInt().orR && RegNext(writingAddr) === getIndex(pipeAddr) && (
      storeJustWrittenData.valid && storeJustWrittenData.tag === getTag(pipeAddr)
    )
  )
  val lookupRdata = Mux(
    storeJustWritten,
    storeJustWrittenData.data(getSublineIdx(pipeAddr)),
    Mux1H(lookups.map(line => (
      line.valid && line.tag === getTag(pipeAddr),
      line.data(getSublineIdx(pipeAddr))
    )))
  )

  val pendingWHits= wbuf.map(buf => buf.valid && buf.aligned === getLineAddr(pipeAddr))
  val pendingWdata = Mux1H(pendingWHits, wbuf.map(_.sdata))
  val pendingBe = Mux1H(pendingWHits, wbuf.map(_.be))

  val rdata = Mux(VecInit(pendingWHits).asUInt.orR, muxBE(pendingBe, pendingWdata, lookupRdata), lookupRdata)

  when(!r.stall) {
    pipeRead := r.read
    pipeReserve := r.reserve
    pipeAddr := r.addr

    queryAddr := r.addr

    assert(RegNext(queryAddr) === pipeAddr)
  }.otherwise {
    queryAddr := pipeAddr
  }

  when(pipeRead && pipeReserve && !r.stall) {
    resValid := true.B
    resCommitted := false.B
    reserved := getTag(pipeAddr) ## getIndex(pipeAddr) ## 0.U(OFFSET_WIDTH.W)
  }

  pendingRead := false.B

  // FIXME: change this one back?
  when(RegNext(toL2.l2req) =/= L2Req.idle) {
    // read port occupied
    r.stall := true.B
  }.elsewhen((!pipeRead) || hit || storeJustWritten) {
    r.stall := false.B
  }.otherwise {
    r.stall := true.B
    pendingRead := true.B
  }

  r.data := rdata

  // L2 Handler
  toL2.l2stall := false.B
  val mainWriting = l1writing.asUInt().orR
  val lookupReady = !mainWriting && RegNext(toL2.l2stall && !mainWriting)
  // FIXME: impl this
  val resCancelDelay = RegInit(0.U(5.W)) // Up to 32

  val l2Rdata = MuxCase(0.U, lookups.map(line => (
    line.valid && line.tag === getTag(toL2.l2addr),
    line.data.asUInt()
  )))

  when(toL2.l2req =/= L2Req.idle) {
    // assert(toL2.l1stall) // L2req can only happen when l2 is processing other ports' requests
    // Nope that may happen. Suppose we are to victimize a line held by this L1

    assert(!toL2.l2addr(IGNORED_WIDTH-1, 0).orR())

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
      toL2.l2stall := false.B
      val hitmask = VecInit(lookups.map(lookup => lookup.valid && lookup.tag === getTag(toL2.l2addr)))
      writing := hitmask
      writingData := written
      writingAddr := getIndex(toL2.l2addr)

      when(toL2.l2req === L2Req.inval && toL2.l2addr === reserved) {
        resValid := false.B
      }
    }
  }
}
