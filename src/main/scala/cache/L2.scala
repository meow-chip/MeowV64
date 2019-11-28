package cache

import chisel3._
import chisel3.MultiIOModule
import chisel3.experimental._
import _root_.data._
import chisel3.util._

/**
 * L2 cache
 * 
 * L2 is composed by two components: the primary component, and the external interface.
 * the primary component handles read/write from the underlying L1 caches
 * the external interface processes AXI requests/responsese
 * 
 * Currently, neither component is pipelined to reduce complexity in impl L2.
 * A round-robin arbiter is included to deal with concurrent coherence protocol requests from L1
 */

trait L2Opts extends L1Opts {
  val L2_SIZE: Int // In bytes
  val L2_ASSOC: Int
  val L2_WB_DEPTH: Int

  val CORE_COUNT: Int

  // TODO: check is log2
  assume(L2_SIZE % LINE_WIDTH == 0)
}

object L2DirState extends ChiselEnum {
  val vacant, shared, modified = Value
}

class L2DirEntry(val opts: L2Opts) extends Bundle {
  val TAIL_LENGTH = log2Ceil(opts.L2_SIZE)
  val TAG_LENGTH = opts.ADDR_WIDTH - TAIL_LENGTH

  val valid = Bool()
  val dirty = Bool()
  val tag = UInt(TAG_LENGTH.W)
  val states = Vec(opts.CORE_COUNT, L2DirState())

  def hit(addr: UInt): Bool = valid && addr(opts.ADDR_WIDTH-1, TAIL_LENGTH) === tag

  def editState(core: UInt, state: L2DirState.Type): L2DirEntry = {
    val w = Wire(new L2DirEntry(opts))
    w := this
    w.states(core) := state
    w
  }
}

object L2DirEntry {
  def default(opts: L2Opts): L2DirEntry = {
    val w = Wire(new L2DirEntry(opts))
    w := DontCare
    w.valid := false.B
    w.dirty := false.B

    w
  }

  def withAddr(opts: L2Opts, addr: UInt): L2DirEntry = {
    val INDEX_OFFSET_LENGTH = log2Ceil(opts.L2_SIZE)

    val w = Wire(new L2DirEntry(opts))
    w.valid := true.B
    w.dirty := false.B
    w.tag := addr >> INDEX_OFFSET_LENGTH
    w.states := VecInit(Seq.fill(opts.CORE_COUNT)(L2DirState.vacant))
    w
  }
}

class L2Assoc(val opts: L2Opts) {
  val lineCount = opts.L2_SIZE / opts.LINE_WIDTH / opts.L2_ASSOC

  val directory = Mem(lineCount, new L2DirEntry(opts))
  val store = SyncReadMem(lineCount, UInt((opts.LINE_WIDTH * 8).W))
}

object L2MainState extends ChiselEnum {
  val reset,
    idle,
    readHit,
    readRefilled,
    modifyInit,
    waitFlush,
    waitInval,
    write
    = Value
}

object L2WBState extends ChiselEnum {
  val idle,
    writing,
    resp = Value
}

class WBEntry(opts: L2Opts) extends Bundle {
  val OFFSET_LENGTH = log2Ceil(opts.LINE_WIDTH)
  val lineaddr = UInt((opts.ADDR_WIDTH - OFFSET_LENGTH).W)
  val data = UInt((opts.LINE_WIDTH*8).W)
}

class L2Cache(val opts: L2Opts) extends MultiIOModule {
  val OFFSET_LENGTH = log2Ceil(opts.LINE_WIDTH)
  val INDEX_OFFSET_LENGTH = log2Ceil(opts.L2_SIZE)

  val i$ = IO(Vec(opts.CORE_COUNT, Flipped(new L1I$Port(opts))))
  val d$ = IO(Vec(opts.CORE_COUNT, Flipped(new L1D$Port(opts))))
  val directs = IO(Vec(opts.CORE_COUNT, Flipped(new L1D$Port(opts))))

  // Iterator for all ports
  // d$ comes first to match MESI directory
  def ports = d$.iterator ++ i$.iterator ++ directs.iterator

  val axi = IO(new AXI(opts.XLEN, opts.ADDR_WIDTH))
  axi <> 0.U.asTypeOf(axi)

  // Assoc and writers
  val assocs = for(_ <- (0 until opts.L2_ASSOC)) yield new L2Assoc(opts)
  val dirWriters = VecInit(assocs.map(assoc => {
    val store = assoc.directory

    val ret = new Bundle {
      val addr = Wire(UInt(opts.ADDR_WIDTH.W))
      val dir = new L2DirEntry(opts)
      val commit = Wire(Bool())
    }

    when(ret.commit) {
      store.write(ret.addr >> OFFSET_LENGTH, ret.dir)
    }

    ret
  }))
  val dataWriters = VecInit(assocs.map(assoc => {
    val store = assoc.store

    val ret = new Bundle {
      val addr = Wire(UInt(opts.ADDR_WIDTH.W))
      val data = Wire(UInt((opts.LINE_WIDTH * 8).W))
      val commit = Wire(Bool())
    }

    when(ret.commit) {
      store.write(ret.addr >> OFFSET_LENGTH, ret.data)
    }

    ret
  }))

  for(d <- dirWriters) {
    d.addr := DontCare
    d.dir := DontCare
    d.commit := false.B
  }
  for(d <- dataWriters) {
    d.addr := DontCare
    d.data := DontCare
    d.commit := false.B
  }

  // Joint ports
  val addrs = Wire(Vec(opts.CORE_COUNT * 3, UInt(opts.ADDR_WIDTH.W)))
  val ops = Wire(Vec(opts.CORE_COUNT * 3, L1D$Port.L1Req()))
  val rdatas = Wire(Vec(opts.CORE_COUNT * 3, L1D$Port.L1Req()))
  val stalls = Wire(Vec(opts.CORE_COUNT * 3, Bool()))
  for((p, a) <- ports.zip(addrs.iterator)) {
    a := p.getAddr
  }

  for((p, o) <- ports.zip(ops.iterator)) {
    o := p.getReq
  }

  for((p, r) <- ports.zip(rdatas.iterator)) {
    p.getRdata := r
  }

  for((p, s) <- ports.zip(stalls.iterator)) {
    p.getStall := s
  }

  rdatas := VecInit(Seq.fill(opts.CORE_COUNT)(DontCare))
  stalls := VecInit(Seq.fill(opts.CORE_COUNT * 3)(true.B))

  // Refillers
  val misses = RegInit(VecInit(Seq.fill(opts.CORE_COUNT * 3)(false.B)))
  val refilled = RegInit(VecInit(Seq.fill(opts.CORE_COUNT * 3)(false.B)))
  val bufs = RegInit(VecInit(Seq.fill(opts.CORE_COUNT * 3)(
    VecInit(Seq.fill(opts.LINE_WIDTH * 8 / axi.DATA_WIDTH)(0.U(axi.DATA_WIDTH.W)))
  )))

  // Writebacks
  val wbFifo = Reg(Vec(opts.L2_WB_DEPTH, new WBEntry(opts)))
  val wbFifoHead = RegInit(0.U(log2Ceil(opts.L2_WB_DEPTH).W))
  val wbFifoTail = RegInit(0.U(log2Ceil(opts.L2_WB_DEPTH).W))
  val wbFifoEmpty = wbFifoHead === wbFifoTail
  val wbFifoFull = wbFifoHead === wbFifoTail +% 1.U

  val wbState = RegInit(L2WBState.idle)

  /* Main loop stuff */

  // Reset
  val rstCnt = RegInit(0.U)
  val rstDir = L2DirEntry.default(opts)

  val state = RegInit(L2MainState.reset)
  val nstate = Wire(L2MainState())

  nstate := state
  state := nstate

  val target = RegInit(0.U(log2Ceil(opts.CORE_COUNT * 3).W))
  val targetUncached = target >= (opts.CORE_COUNT * 2).U
  val targetAddr = addrs(target)
  val targetOps = ops(target)
  val targetIndex = targetAddr(INDEX_OFFSET_LENGTH-1, OFFSET_LENGTH)

  // Compute directory lookups & delayed data fetch
  val lookups = VecInit(assocs.map(_.directory.read(targetIndex)))
  val datas = assocs.map(_.store.read(targetIndex))
  val pipeLookups = RegNext(lookups)

  // Randomly picks victim, even if it's not full yet, because I'm lazy
  // TODO: PLRU, and fill in blanks
  val rand = chisel3.util.random.LFSR(8)
  val victim = RegInit(0.U(log2Ceil(opts.L2_ASSOC).W))

  // Downlink requests
  val pendings = RegInit(VecInit(Seq.fill(opts.CORE_COUNT)(L1D$Port.L2Req.idle)))
  val pending = pendings.foldLeft(false.B)((acc, req) => acc || req =/= L1D$Port.L2Req.idle)
  val pendingVictim = RegInit(false.B)

  for((d, p) <- d$.iterator.zip(pendings.iterator)) {
    d.l2req := p
    when(pendingVictim) {
      d.l2addr := pipeLookups(victim).tag ## targetAddr(INDEX_OFFSET_LENGTH-1, 0)
    }.otherwise {
      d.l2addr := targetAddr
    }
  }

  def step(ptr: UInt) {
    when(ptr === (opts.CORE_COUNT * 3 - 1).U) {
      ptr := 0.U
    }.otherwise {
      ptr := ptr + 1.U
    }
  }

  switch(state) {
    is(L2MainState.reset) {
      for(dir <- dirWriters) {
        dir.addr := rstCnt ## 0.U(OFFSET_LENGTH.W)
        dir.dir := rstDir
        dir.commit := true.B
      }

      rstCnt := rstCnt + 1.U

      when(rstCnt === (opts.L2_SIZE / opts.LINE_WIDTH / opts.L2_ASSOC).U) {
        nstate := L2MainState.idle
      }
    }

    is(L2MainState.idle) {
      // TODO: pipelining
      // TODO: check refill

      // TODO: check directory

      switch(targetOps) {
        is(L1D$Port.L1Req.idle) {
          nstate := L2MainState.idle

          step(target)
        }

        is(L1D$Port.L1Req.read, L1D$Port.L1Req.readWrite) {
          val hit = (!targetUncached) && lookups.foldLeft(false.B)((acc, l) => acc || l.hit(targetAddr))

          when(misses(target)) {
            when(refilled(target)) {
              when(targetUncached) {
                val rdata = bufs(target).asTypeOf(UInt(opts.LINE_WIDTH.W))
                rdatas(target) := rdata
                stalls(target) := false.B

                misses(target) := false.B
                refilled(target) := false.B

                nstate := L2MainState.idle
                step(target)
              }.otherwise {
                nstate := L2MainState.readRefilled
              }
            }.otherwise {
              nstate := L2MainState.idle
              step(target)
            }
          }.elsewhen(hit) {
            nstate := L2MainState.readHit
          }.otherwise {
            // hit == false && missed == false
            // Init a refill
            misses(target) := true.B
            refilled(target) := false.B

            nstate := L2MainState.idle
            step(target)
          }
        }

        is(L1D$Port.L1Req.modify) {
          nstate := L2MainState.modifyInit
        }

        is(L1D$Port.L1Req.writeback) {
          // FIXME: impl
        }
      }
    }

    is(L2MainState.readHit) {
      val (hit, data) = lookups.zip(datas).foldRight((false.B, 0.U))((cur, acc) => {
        val hit = cur._1.hit(targetAddr)
        val line = Wire(UInt())
        when(hit) {
          line := cur._2
        }.otherwise {
          line := 0.U
        }

        (acc._1 || hit, acc._2 | line)
      })

      // Asserts hit

      nstate := L2MainState.idle
      step(target)

      rdatas(target) := data
      stalls(target) := false.B // Unstall for one cycle
      misses(target) := false.B
      refilled(target) := false.B
    }

    is(L2MainState.readRefilled) {
      val curVictim = rand(log2Ceil(opts.L2_ASSOC)-1, 0)
      victim := curVictim

      def commit() = {
        val rdata = bufs(target).asTypeOf(UInt(opts.LINE_WIDTH.W))
        // TODO: check if CORE_COUNT = 2^n
        val coreWidth = log2Ceil(opts.CORE_COUNT)
        dirWriters(curVictim).addr := targetAddr
        dirWriters(curVictim).dir :=
          L2DirEntry.withAddr(opts, targetAddr).editState(target(coreWidth-1, 0), L2DirState.shared)

        dataWriters(curVictim).addr := targetAddr
        dataWriters(curVictim).data := rdata
        dataWriters(curVictim).commit := true.B

        rdatas(target) := rdata
        stalls(target) := false.B

        misses(target) := false.B
        refilled(target) := false.B

        step(target)
      }

      when(!lookups(curVictim).valid) {
        commit()
      }.otherwise {
        val hasDirty = lookups(curVictim).states.foldLeft(false.B)((acc, s) => acc || s === L2DirState.modified)
        val hasShared = lookups(curVictim).states.foldLeft(false.B)((acc, s) => acc || s =/= L2DirState.vacant)

        when(hasDirty) {
          pendingVictim := true.B

          for((s, p) <- lookups(curVictim).states.zip(pendings)) {
            when(s === L2DirState.modified) {
              p := L1D$Port.L2Req.flush
            }
          }

          nstate := L2MainState.waitFlush
        }.elsewhen(hasShared) {
          pendingVictim := true.B

          for((s, p) <- lookups(curVictim).states.zip(pendings)) {
            when(s === L2DirState.modified) {
              p := L1D$Port.L2Req.inval
            }
          }

          nstate := L2MainState.waitInval
        }.otherwise {
          // TODO: writeback
          commit()
        }
      }
    }

    is(L2MainState.modifyInit) {
      val ent = pipeLookups.foldLeft(0.U.asTypeOf(new L2DirEntry(opts)))((acc, ent) => {
        val h = ent.hit(targetAddr)
        val mask = Wire(new L2DirEntry(opts))
        when(h) {
          mask := 0.U.asTypeOf(mask)
        }.otherwise {
          mask := ent
        }

        (acc.asUInt | ent.asUInt).asTypeOf(acc)
      })

      // TODO: asserts ent.valid

      val hasDirty = ent.states.foldLeft(false.B)((acc, s) => acc || s === L2DirState.modified)
      val hasShared = ent.states.foldLeft(false.B)((acc, s) => acc || s =/= L2DirState.vacant)

      when(hasDirty) {
        for((s, p) <- ent.states.zip(pendings)) {
          when(s === L2DirState.modified) {
            p := L1D$Port.L2Req.flush
          }
        }

        pendingVictim := false.B
        nstate := L2MainState.waitFlush
      }.otherwise {
        for((s, p) <- ent.states.zip(pendings)) {
          when(s =/= L2DirState.vacant) {
            p := L1D$Port.L2Req.inval
          }
        }

        pendingVictim := false.B
        nstate := L2MainState.waitFlush
      }
    }

    is(L2MainState.waitFlush) {
      val ent = Wire(new L2DirEntry(opts))

      when(pendingVictim) {
        ent := pipeLookups(victim)
      }.otherwise {
        // TODO: pipe hit
        ent := pipeLookups.foldLeft(0.U.asTypeOf(new L2DirEntry(opts)))((acc, ent) => {
          val h = ent.hit(targetAddr)
          val mask = Wire(new L2DirEntry(opts))
          when(h) {
            mask := 0.U.asTypeOf(mask)
          }.otherwise {
            mask := ent
          }

          (acc.asUInt | ent.asUInt).asTypeOf(acc)
        })
      }

      for((d, p) <- d$.zip(pendings)) {
        // TODO: mark dirty
        when(!d.l2stall) {
          // FIXME: handle write
          p := L1D$Port.L2Req.idle
        }
      }

      when(!pending) {
        for((s, p) <- ent.states.zip(pendings)) {
          when(s =/= L2DirState.vacant) {
            p := L1D$Port.L2Req.inval
          }
        }

        nstate := L2MainState.waitInval
      }
    }

    is(L2MainState.waitInval) {
      for((d, p) <- d$.zip(pendings)) {
        when(!d.l2stall) {
          p := L1D$Port.L2Req.idle
        }
      }

      when(!pending) {
        when(pendingVictim) { // Refilled
          val rdata = bufs(target).asTypeOf(UInt(opts.LINE_WIDTH.W))
          // TODO: check if CORE_COUNT = 2^n
          val coreWidth = log2Ceil(opts.CORE_COUNT)

          dirWriters(victim).addr := targetAddr
          dirWriters(victim).dir := L2DirEntry.withAddr(opts, targetAddr).editState(target(coreWidth-1, 0), L2DirState.shared)
          dirWriters(victim).commit := true.B

          dataWriters(victim).addr := targetAddr
          dataWriters(victim).data := rdata
          dataWriters(victim).commit := true.B

          rdatas(target) := rdata
          stalls(target) := false.B

          misses(target) := false.B
          refilled(target) := false.B
        }.otherwise {
          for(lookup <- lookups) {
            when(lookup.hit(targetAddr)) {
              val coreWidth = log2Ceil(opts.CORE_COUNT)
              val modified = Wire(lookup.cloneType)
              modified := lookup
              modified.states := VecInit(Seq.fill(opts.CORE_COUNT)(L2DirState.vacant))
              modified.states(target(coreWidth-1, 0)) := L2DirState.modified

              lookup := modified
            }
          }
        }

        nstate := L2MainState.idle
        step(target)
      }
    }
  }

  // Refiller, only deals with AR channel
  val axiGrpNum = opts.LINE_WIDTH * 8 / axi.DATA_WIDTH

  val reqTarget = RegInit(0.U(log2Ceil(opts.CORE_COUNT * 3).W))
  val rawReqAddr = addrs(reqTarget)

  val reqAddr = rawReqAddr(opts.ADDR_WIDTH-1, OFFSET_LENGTH) ## 0.U(OFFSET_LENGTH.W)
  assume(reqAddr.getWidth == opts.ADDR_WIDTH)
  
  val sent = RegInit(VecInit(Seq.fill(opts.CORE_COUNT * 3)(false.B)))

  when(sent(reqTarget) != misses(reqTarget)) {
    when(misses(reqTarget)) {
      axi.ARADDR := reqAddr
      axi.ARBURST := AXI.Constants.Burst.INCR.U
      // axi.ARCACHE ignored
      // Yeah.. we are finally using id
      axi.ARID := reqTarget
      assume(
        opts.LINE_WIDTH * 8 % axi.DATA_WIDTH == 0,
        "Line cannot be filled with a integer number of AXI transfers"
      )
      axi.ARLEN := (axiGrpNum - 1).U
      // axi.ARPROT ignored
      // axi.ARQOS ignored
      // axi.ARREGION ignored
      axi.ARSIZE := AXI.Constants.Size.from(axi.DATA_WIDTH).U

      axi.ARVALID := true.B
      when(axi.ARREADY) {
        sent(reqTarget) := true.B
        step(reqTarget)
      }
    } .otherwise {
      sent(reqTarget) := false.B
      step(reqTarget)
    }
  }.otherwise {
    // Step
    step(reqTarget)
  }

  // Receiver, deals with R channel, and victimize
  val bufptrs = RegInit(VecInit(Seq.fill(opts.CORE_COUNT * 2)(
    0.U(log2Ceil(opts.LINE_WIDTH * 8 / axi.DATA_WIDTH).W)
  )))

  when(axi.RVALID) {
    // TODO: handle RRESP
    axi.RREADY := true.B

    bufs(axi.RID)(bufptrs(axi.RID)) := axi.RDATA
    bufptrs(axi.RID) := bufptrs(axi.RID) + 1.U

    when(axi.RLAST) {
      refilled(axi.RID) := true.B
    }
  }

  // Write-back...er? Handles AXI AW/W/B
  val wbPtr = RegInit(0.U(log2Ceil(axiGrpNum).W))
  val wbDataView = wbFifo(wbFifoHead).data.asTypeOf(Vec(axiGrpNum, UInt(axi.DATA_WIDTH.W)))
  switch(wbState) {
    is(L2WBState.idle) {
      when(!wbFifoEmpty) {
        axi.AWID := 0.U
        axi.AWADDR := wbFifo(wbFifoHead).lineaddr ## 0.U(OFFSET_LENGTH)
        axi.AWBURST := AXI.Constants.Burst.INCR.U
        axi.AWCACHE := 0.U
        axi.AWLEN := (axiGrpNum - 1).U
        axi.AWPROT := 0.U
        axi.AWQOS := 0.U
        axi.AWREGION := 0.U
        axi.AWSIZE := AXI.Constants.Size.from(axi.DATA_WIDTH).U
        axi.AWVALID := true.B

        when(axi.AWREADY) {
          wbPtr := 0.U
          wbState := L2WBState.writing
        }
      }
    }

    is(L2WBState.writing) {
      axi.WDATA := wbDataView(wbPtr)
      axi.WSTRB := ((1 << (axi.DATA_WIDTH / 8))-1).U
      axi.WVALID := true.B

      val isLast = wbPtr === axiGrpNum.U
      axi.WLAST := isLast

      when(axi.WREADY) {
        wbPtr := wbPtr +% 1.U

        when(isLast) {
          wbState := L2WBState.resp
        }
      }
    }

    is(L2WBState.resp) {
      // TODO: poison the cache if an error occurred

      axi.BREADY := true.B
      when(axi.BVALID) {
        wbFifoHead := wbFifoHead +% 1.U
        wbState := L2WBState.idle
      }
    }
  }
}
