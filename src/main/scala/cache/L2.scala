package cache

import chisel3._
import chisel3.MultiIOModule
import chisel3.experimental._
import _root_.data._
import chisel3.util._
import cache.L1DCPort.L2Req
import cache.L1DCPort.L1Req
import interrupt._

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
  val WB_DEPTH: Int
  val CORE_COUNT: Int

  val MMIO: Seq[MMIOMapping]
}

object L2DirState extends ChiselEnum {
  val vacant, shared, modified = Value
}

class L2DirEntry(val opts: L2Opts) extends Bundle {
  val INDEX_OFFSET_LENGTH = log2Ceil(opts.SIZE / opts.ASSOC)
  val TAG_LENGTH = opts.ADDR_WIDTH - INDEX_OFFSET_LENGTH

  val valid = Bool()
  val dirty = Bool()
  val tag = UInt(TAG_LENGTH.W)
  val states = Vec(opts.CORE_COUNT, L2DirState())

  def hit(addr: UInt): Bool = valid && addr(opts.ADDR_WIDTH-1, INDEX_OFFSET_LENGTH) === tag

  def editState(core: UInt, state: L2DirState.Type): L2DirEntry = {
    val w = Wire(new L2DirEntry(opts))
    w := this
    w.states(core) := state
    w
  }

  def withDirty(): L2DirEntry = {
    val w = Wire(new L2DirEntry(opts))
    w := this
    w.dirty := true.B
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
    val INDEX_OFFSET_LENGTH = log2Ceil(opts.SIZE / opts.ASSOC)

    val w = Wire(new L2DirEntry(opts))
    w.valid := true.B
    w.dirty := false.B
    w.tag := addr >> INDEX_OFFSET_LENGTH
    w.states := VecInit(Seq.fill(opts.CORE_COUNT)(L2DirState.vacant))
    w
  }
}

object L2MainState extends ChiselEnum {
  val reset,
    idle,
    hit,
    refilled,
    waitFlush,
    waitInval
    = Value
}

object L2WBState extends ChiselEnum {
  val idle,
    writing,
    resp,
    ucWalk
    = Value
}

class WBEntry(val opts: L2Opts) extends Bundle {
  val OFFSET_LENGTH = log2Ceil(opts.LINE_WIDTH)
  val lineaddr = UInt((opts.ADDR_WIDTH - OFFSET_LENGTH).W)
  val data = UInt((opts.LINE_WIDTH*8).W)
  val valid = Bool()
}

object WBEntry {
  def default(opts: L2Opts): WBEntry = {
    val ret = Wire(new WBEntry(opts))
    ret.lineaddr := DontCare
    ret.data := DontCare
    ret.valid := false.B

    ret
  }
}

@chiselName
class L2Cache(val opts: L2Opts) extends MultiIOModule {
  val OFFSET_LENGTH = log2Ceil(opts.LINE_WIDTH)
  val INDEX_OFFSET_LENGTH = log2Ceil(opts.SIZE / opts.ASSOC)
  val INDEX_LENGTH = INDEX_OFFSET_LENGTH - OFFSET_LENGTH
  val TAG_LENGTH = opts.ADDR_WIDTH - INDEX_OFFSET_LENGTH
  val ASSOC_IDX_WIDTH = log2Ceil(opts.ASSOC)
  val LINE_COUNT = opts.SIZE / opts.LINE_WIDTH / opts.ASSOC

  object GeneralMMIODef extends {
    override val ADDR_WIDTH: Int = opts.ADDR_WIDTH
    override val XLEN: Int = opts.XLEN
  } with MMIODef

  val ic = IO(Vec(opts.CORE_COUNT, Flipped(new L1ICPort(opts))))
  val dc = IO(Vec(opts.CORE_COUNT, Flipped(new L1DCPort(opts))))
  val directs = IO(Vec(opts.CORE_COUNT, Flipped(new L1UCPort(opts))))
  val mmio = IO(Vec(
    opts.MMIO.size,
    Flipped(new MMIOAccess(GeneralMMIODef))
  ))

  for(m <- mmio) {
    m.req.noenq()
  }

  // Iterator for all ports
  // dc comes first to match MSI directory
  def ports = dc.iterator ++ ic.iterator

  val axi = IO(new AXI(opts.XLEN, opts.ADDR_WIDTH))
  axi := DontCare
  axi.ARVALID := false.B
  axi.RREADY := false.B
  axi.AWVALID := false.B
  axi.WVALID := false.B
  axi.BREADY := false.B

  // Assoc and writers
  val directories = Mem(LINE_COUNT, Vec(opts.ASSOC, new L2DirEntry(opts)))
  val stores = SyncReadMem(LINE_COUNT, Vec(opts.ASSOC, UInt((opts.LINE_WIDTH * 8).W)))

  val dataWriter = Wire(new Bundle {
    val mask = Vec(opts.ASSOC, Bool())
    val addr = UInt(INDEX_LENGTH.W)
    val data = UInt((opts.LINE_WIDTH * 8).W)
  })

  val dirWriter = Wire(new Bundle {
    val mask = Vec(opts.ASSOC, Bool())
    val addr = UInt(INDEX_LENGTH.W)
    val dir = new L2DirEntry(opts)
  })

  dataWriter := DontCare
  dataWriter.mask := VecInit(Seq.fill(4)(false.B))
  dirWriter := DontCare
  dirWriter.mask := VecInit(Seq.fill(4)(false.B))

  stores.write(dataWriter.addr, VecInit(Seq.fill(opts.ASSOC)(dataWriter.data)), dataWriter.mask)
  directories.write(dirWriter.addr, VecInit(Seq.fill(opts.ASSOC)(dirWriter.dir)), dirWriter.mask)

  def writeData(idx: UInt, addr: UInt, data: UInt) {
    assume(data.getWidth == opts.LINE_WIDTH * 8)

    val mask = Wire(Vec(opts.ASSOC, Bool()))
    for((m, i) <- mask.zipWithIndex) {
      m := i.U === idx
    }

    dataWriter.addr := addr(INDEX_OFFSET_LENGTH-1, OFFSET_LENGTH)
    dataWriter.data := data
    dataWriter.mask := mask
  }

  def writeDir(idx: UInt, addr: UInt, dir: L2DirEntry) {
    val mask = Wire(Vec(opts.ASSOC, Bool()))
    for((m, i) <- mask.zipWithIndex) {
      m := i.U === idx
    }

    dirWriter.addr := addr(INDEX_OFFSET_LENGTH-1, OFFSET_LENGTH)
    dirWriter.dir := dir
    dirWriter.mask := mask
  }

  def writeDirAll(addr: UInt, dir: L2DirEntry) {
    val mask = Wire(Vec(opts.ASSOC, Bool()))
    for((m, i) <- mask.zipWithIndex) {
      m := true.B
    }

    dirWriter.addr := addr(INDEX_OFFSET_LENGTH-1, OFFSET_LENGTH)
    dirWriter.dir := dir
    dirWriter.mask := mask
  }

  // Joint ports
  val addrs = Wire(Vec(opts.CORE_COUNT * 2, UInt(opts.ADDR_WIDTH.W)))
  val ops = Wire(Vec(opts.CORE_COUNT * 2, L1DCPort.L1Req()))
  val rdatas = Wire(Vec(opts.CORE_COUNT * 2, UInt((opts.LINE_WIDTH * 8).W)))
  val stalls = Wire(Vec(opts.CORE_COUNT * 2, Bool()))
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

  for(r <- rdatas) r := DontCare
  stalls := VecInit(ops.map(op => op =/= L1Req.idle))
  for(uc <- directs) {
    uc.stall := uc.read || uc.write
    uc.rdata := DontCare
  }

  /* Uncached ports */
  def isInMapping(addr: UInt, mapping: MMIOMapping) = {
    val ret = WireDefault(false.B)
    when(
      addr >= mapping.MAPPED_START.U(opts.XLEN.W)(opts.ADDR_WIDTH-1, 0)
      && addr < (mapping.MAPPED_START + mapping.MAPPED_SIZE).U(opts.XLEN.W)(opts.ADDR_WIDTH-1, 0)
    ) {
      ret := true.B
    }
    ret
  }

  def getMMIOMap(addr: UInt) = VecInit(opts.MMIO.map((mapping) => isInMapping(addr, mapping))).asUInt()
  def isMMIO(addr: UInt) = getMMIOMap(addr).orR()

  // Refillers
  val misses = RegInit(VecInit(Seq.fill(opts.CORE_COUNT * 2)(false.B)))
  val missesAddr = RegInit(VecInit(Seq.fill(opts.CORE_COUNT * 2)(0.U(opts.ADDR_WIDTH.W)))) // Pipe addr to optimize timing
  // val collided = RegInit(VecInit(Seq.fill(opts.CORE_COUNT * 2)(false.B)))
  val refilled = RegInit(VecInit(Seq.fill(opts.CORE_COUNT * 2)(false.B)))
  val sent = RegInit(VecInit(Seq.fill(opts.CORE_COUNT * 2)(false.B)))
  val ucSent = RegInit(VecInit(Seq.fill(opts.CORE_COUNT)(false.B)))
  val bufs = RegInit(VecInit(Seq.fill(opts.CORE_COUNT * 2)(
    VecInit(Seq.fill(opts.LINE_WIDTH * 8 / axi.DATA_WIDTH)(0.U(axi.DATA_WIDTH.W)))
  )))

  for((m, r) <- misses.zip(refilled)) assert(m || !r) // refilled implies miss
  // for((m, c) <- misses.zip(collided)) assert(!(m && c)) // miss and collided is never true at the same time

  // Writebacks
  val wbFifo = RegInit(VecInit(Seq.fill(opts.WB_DEPTH)(WBEntry.default(opts))))
  val wbFifoHead = RegInit(0.U(log2Ceil(opts.WB_DEPTH).W))
  val wbFifoTail = RegInit(0.U(log2Ceil(opts.WB_DEPTH).W))
  val wbFifoEmpty = wbFifoHead === wbFifoTail
  val wbFifoFull = wbFifoHead === wbFifoTail +% 1.U

  val wbState = RegInit(L2WBState.idle)

  /* Main loop stuff */

  // Reset
  val rstCnt = RegInit(0.U((INDEX_OFFSET_LENGTH - OFFSET_LENGTH).W))
  val rstDir = L2DirEntry.default(opts)

  val state = RegInit(L2MainState.reset)
  val nstate = Wire(L2MainState())

  nstate := state
  state := nstate

  val target = RegInit(0.U(log2Ceil(opts.CORE_COUNT * 2).W))
  val targetAddr = addrs(target)
  val targetOps = ops(target)
  val targetIndex = targetAddr(INDEX_OFFSET_LENGTH-1, OFFSET_LENGTH)
  val pipeTargetAddr = RegNext(targetAddr)

  val sameAddrRefilling = VecInit((0 until opts.CORE_COUNT * 2).map(
    idx => misses(idx) && addrs(idx) === targetAddr
  )).asUInt().orR

  // Compute directory lookups & delayed data fetch
  val lookups = directories.read(targetIndex)
  val datas = stores.read(targetIndex)
  val pipeLookups = RegNext(lookups)
  val hits = lookups.map(_.hit(targetAddr))
  val hit = VecInit(hits).asUInt().orR
  val pipeHit = RegNext(hit)
  val hitCount = PopCount(hits)
  assert(hitCount <= 1.U)
  val hitIdx = OHToUInt(hits)
  val pipeHitIdx = RegNext(hitIdx)

  // Refilling from FIFO
  val fifoHits = wbFifo.map(ent => ent.valid && ent.lineaddr === targetAddr >> OFFSET_LENGTH)
  val fifoHitCnt = PopCount(fifoHits)
  assert(fifoHitCnt <= 1.U)
  val fifoHit = VecInit(fifoHits).asUInt().orR
  val fifoHitData = Mux1H(fifoHits, wbFifo.map(_.data))

  // Randomly picks victim, even if it's not full yet, because I'm lazy
  // TODO: PLRU, and fill in blanks
  val rand = chisel3.util.random.LFSR(8)
  val victim = RegInit(0.U(log2Ceil(opts.ASSOC).W))

  // Downlink requests
  val pendings = RegInit(VecInit(Seq.fill(opts.CORE_COUNT)(L1DCPort.L2Req.idle)))
  val pending = VecInit(pendings.map(_ =/= L1DCPort.L2Req.idle)).asUInt.orR
  val pendingVictim = RegInit(false.B)

  for((d, p) <- dc.iterator.zip(pendings.iterator)) {
    d.l2req := p
    // TODO: investigate can we use pipeTargetAddr here
    when(pendingVictim) {
      d.l2addr := pipeLookups(victim).tag ## pipeTargetAddr(INDEX_OFFSET_LENGTH-1, 0)
    }.otherwise {
      d.l2addr := pipeTargetAddr
    }
  }

  def stepRefiller(ptr: UInt) {
    when(ptr === (opts.CORE_COUNT * 3 - 1).U) {
      ptr := 0.U
    }.otherwise {
      ptr := ptr + 1.U
    }
  }

  val nextEventful = Wire(UInt(log2Ceil(opts.CORE_COUNT * 2).W))
  nextEventful := MuxCase(target, (1 until opts.CORE_COUNT * 2).map(i => {
    val p = Mux(target < (opts.CORE_COUNT * 2 - i).U, target + i.U, target - (opts.CORE_COUNT*2 - i).U)

    (
      (!misses(p) || refilled(p)) && ops(p) =/= L1Req.idle,
      p
    )
  }))

  def step() {
    target := nextEventful
  }

  switch(state) {
    is(L2MainState.reset) {
      writeDirAll(0.U(TAG_LENGTH.W) ## rstCnt ## 0.U(OFFSET_LENGTH.W), rstDir)

      rstCnt := rstCnt + 1.U

      when(rstCnt.andR) {
        nstate := L2MainState.idle
      }
    }

    is(L2MainState.idle) {
      // TODO: pipelining

      switch(targetOps) {
        is(L1DCPort.L1Req.idle) {
          nstate := L2MainState.idle

          step()
        }

        is(L1DCPort.L1Req.read, L1DCPort.L1Req.modify) {
          when(misses(target)) {
            when(refilled(target)) {
              victim := rand(log2Ceil(opts.ASSOC)-1, 0)
              nstate := L2MainState.refilled
            }.otherwise {
              nstate := L2MainState.idle
              step()
            }
          }.elsewhen(hit) {
            nstate := L2MainState.hit
          }.elsewhen(sameAddrRefilling) { // Someone-else is fetching for me, and it's not a hit yet
            nstate := L2MainState.idle
            step()
          }.otherwise {
            assert(!(fifoHit && sameAddrRefilling)) // FIFO inhabitance and AXI refilling are mutually exclusive

            // hit == false && missed == false
            // Init a refill

            refilled(target) := false.B

            when(fifoHit) {
              // Refill from FIFO
              bufs(target) := fifoHitData.asTypeOf(bufs(target))
              missesAddr(target) := addrs(target)

              misses(target) := true.B
              sent(target) := true.B
              refilled(target) := true.B // Immediately refills

              victim := rand(log2Ceil(opts.ASSOC)-1, 0)
              nstate := L2MainState.refilled
            }.otherwise {
              misses(target) := true.B
              missesAddr(target) := addrs(target)
              sent(target) := false.B
            }

            nstate := L2MainState.idle
            step()
          }
        }

        is(L1DCPort.L1Req.writeback) {
          assert(lookups(hitIdx).hit(targetAddr))
          for(core <- (0 until opts.CORE_COUNT)) {
            when(core.U === target) {
              assert(lookups(hitIdx).states(core) === L2DirState.modified)
            }.otherwise {
              assert(lookups(hitIdx).states(core) === L2DirState.vacant)
            }
          }

          val writtenDir = L2DirEntry
            .withAddr(opts, targetAddr)
            .editState(target, L2DirState.vacant)
            .withDirty()
          val writtenData = dc(target).wdata

          writeDir(
            hitIdx,
            targetAddr,
            writtenDir
          )

          writeData(
            hitIdx,
            targetAddr,
            writtenData
          )

          stalls(target) := false.B

          nstate := L2MainState.idle

          // We're probably going to receive a read after a write-back, so don't step target
          // step(target)
        }
      }
    }

    is(L2MainState.hit) {
      val data = datas(pipeHitIdx)
      val isDC = target < opts.CORE_COUNT.U

      val dirtyMap = VecInit(lookups(pipeHitIdx).states.map(_ === L2DirState.modified)).asUInt
      val sharedMap = VecInit(lookups(pipeHitIdx).states.map(_ =/= L2DirState.vacant)).asUInt
      val hasDirty = (dirtyMap & ~UIntToOH(target)).asUInt.orR
      val hasShared = (sharedMap & ~UIntToOH(target)).asUInt.orR

      when(!hasShared) {
        assert(!misses(target))

        rdatas(target) := data
        stalls(target) := false.B
        // collided(target) := false.B

        val newState = Wire(L2DirState())

        when(targetOps === L1DCPort.L1Req.read) {
          newState := L2DirState.shared
        }.otherwise {
          newState := L2DirState.modified
        }

        when(isDC) {
          writeDir(
            pipeHitIdx,
            targetAddr,
            lookups(pipeHitIdx).editState(target, newState)
          )
        }

        nstate := L2MainState.idle
        step()
      }.elsewhen(!hasDirty) {
        // If modyfing, we need to inval them first
        when(targetOps === L1DCPort.L1Req.read) {
          assert(!misses(target))

          rdatas(target) := data
          stalls(target) := false.B
          // collided(target) := false.B

          when(isDC) {
            writeDir(
              pipeHitIdx,
              targetAddr,
              lookups(pipeHitIdx).editState(target, L2DirState.shared)
            )
          }

          nstate := L2MainState.idle
        }.otherwise {
          pendingVictim := false.B
          nstate := L2MainState.waitInval

          for(((s, p), idx) <- lookups(pipeHitIdx).states.zip(pendings).zipWithIndex) {
            when(idx.U =/= target && s =/= L2DirState.vacant) {
              p := L1DCPort.L2Req.inval
            }
          }
        }
      }.otherwise {
        pendingVictim := false.B
        nstate := L2MainState.waitFlush

        for(((s, p), idx) <- lookups(pipeHitIdx).states.zip(pendings).zipWithIndex) {
          when(idx.U =/= target && s === L2DirState.modified) {
            p := L1DCPort.L2Req.flush
          }
        }
      }
    }

    is(L2MainState.refilled) {
      val isDC = target < opts.CORE_COUNT.U

      def commit() = {
        val rdata = bufs(target).asTypeOf(UInt((opts.LINE_WIDTH * 8).W))

        assert(misses(target))
        assert(refilled(target))

        rdatas(target) := rdata
        stalls(target) := false.B
        misses(target) := false.B
        refilled(target) := false.B

        val newState = Wire(L2DirState())
        when(targetOps === L1DCPort.L1Req.read) {
          newState := L2DirState.shared
        }.otherwise {
          newState := L2DirState.modified
        }

        when(isDC) {
          writeDir(
            victim,
            targetAddr,
            L2DirEntry.withAddr(opts, targetAddr).editState(target, newState)
          )
        }.otherwise {
          writeDir(
            victim,
            targetAddr,
            L2DirEntry.withAddr(opts, targetAddr)
          )
        }

        writeData(
          victim,
          targetAddr,
          rdata
        )

        nstate := L2MainState.idle
        step()
      }

      when(!lookups(victim).valid) {
        commit()
      }.otherwise {
        val hasDirty = VecInit(lookups(victim).states.map(_ === L2DirState.modified)).asUInt().orR
        val hasShared = VecInit(lookups(victim).states.map(_ =/= L2DirState.vacant)).asUInt().orR

        when(hasDirty) {
          pendingVictim := true.B

          for((s, p) <- lookups(victim).states.zip(pendings)) {
            when(s === L2DirState.modified) {
              p := L1DCPort.L2Req.flush
            }
          }

          nstate := L2MainState.waitFlush
        }.elsewhen(hasShared) {
          pendingVictim := true.B

          for((s, p) <- lookups(victim).states.zip(pendings)) {
            when(s === L2DirState.modified) {
              p := L1DCPort.L2Req.inval
            }
          }

          nstate := L2MainState.waitInval
        }.elsewhen(lookups(victim).dirty) {
          // Is an dirty entry, place into wb fifo

          // Wait until not full
          when(!wbFifoFull) {
            wbFifo(wbFifoTail).lineaddr := lookups(victim).tag ## targetAddr(INDEX_OFFSET_LENGTH-1, OFFSET_LENGTH)
            // Datas should be ready now, after one cycle after jumped from idle
            wbFifo(wbFifoTail).data := datas(victim)
            wbFifo(wbFifoTail).valid := true.B

            wbFifoTail := wbFifoTail +% 1.U

            commit()
          }
        }.otherwise {
          // Not dirty, commit directly
          commit()
        }
      }
    }

    is(L2MainState.waitFlush) {
      val ent = Wire(new L2DirEntry(opts))
      val writtenTowards = Wire(UInt(ASSOC_IDX_WIDTH.W))

      when(pendingVictim) {
        ent := pipeLookups(victim)
        writtenTowards := victim
      }.otherwise {
        ent := lookups(pipeHitIdx)
        writtenTowards := pipeHitIdx
      }

      val writtenAddr = ent.tag ## targetAddr(INDEX_OFFSET_LENGTH-1, 0)
      val writtenData = Wire(UInt((opts.LINE_WIDTH * 8).W))
      val commit = Wire(Bool())
      writtenData := DontCare
      commit := false.B

      for((d, p) <- dc.zip(pendings)) {
        when(p =/= L1DCPort.L2Req.idle && !d.l2stall) {
          p := L1DCPort.L2Req.idle

          writtenData := d.wdata
          commit := true.B
        }
      }

      when(commit) {
        writeData(writtenTowards, writtenAddr, writtenData)
      }

      when(!pending) {
        // At most one of them is in modified state, so the last cycle should be the one
        // in which we committed the changes
        assert(RegNext(commit))

        when(!pendingVictim && targetOps === L1DCPort.L1Req.read) {
          // plain old read hit, after flush we can return our data

          rdatas(target) := datas(pipeHitIdx) // One cycle after data-write, this should be ready
          stalls(target) := false.B
          misses(target) := false.B
          refilled(target) := false.B

          writeDir(pipeHitIdx, targetAddr, lookups(pipeHitIdx).editState(target, L2DirState.shared).withDirty())

          nstate := L2MainState.idle
          step()
        }.otherwise {
          for((s, p) <- ent.states.zip(pendings)) {
            when(s =/= L2DirState.vacant) {
              p := L1DCPort.L2Req.inval
            }
          }

          nstate := L2MainState.waitInval
        }
      }
    }

    is(L2MainState.waitInval) {
      for((d, p) <- dc.zip(pendings)) {
        when(p =/= L1DCPort.L2Req.idle && !d.l2stall) {
          p := L1DCPort.L2Req.idle
        }
      }

      when(!pending) {
        when(pendingVictim) { // Refilled
          // Was dirty
          val hasDirty = VecInit(pipeLookups(victim).states.map(_ === L2DirState.modified)).asUInt.orR
          val isDC = target < opts.CORE_COUNT.U

          def commit() = {
            val rdata = bufs(target).asTypeOf(UInt((opts.LINE_WIDTH * 8).W))
            // TODO: check if CORE_COUNT = 2^n
            val coreWidth = if(opts.CORE_COUNT != 1) { log2Ceil(opts.CORE_COUNT) } else { 1 }

            val sinit = Wire(L2DirState())
            when(!isDC) {
              sinit := L2DirState.vacant
            }.elsewhen(targetOps === L1DCPort.L1Req.read) {
              sinit := L2DirState.shared
            }.otherwise {
              sinit := L2DirState.modified
            }

            writeDir(
              victim,
              targetAddr,
              L2DirEntry.withAddr(opts, targetAddr).editState(target(coreWidth-1, 0), sinit)
            )

            writeData(
              victim,
              targetAddr,
              rdata
            )

            rdatas(target) := rdata
            stalls(target) := false.B
            misses(target) := false.B
            refilled(target) := false.B

            nstate := L2MainState.idle
          }

          when(pipeLookups(victim).dirty || hasDirty) {
            when(!wbFifoFull) {
              wbFifo(wbFifoTail).lineaddr := lookups(victim).tag ## targetAddr(INDEX_OFFSET_LENGTH-1, OFFSET_LENGTH)
              wbFifo(wbFifoTail).data := datas(victim)
              wbFifo(wbFifoTail).valid := true.B
              wbFifoTail := wbFifoTail +% 1.U

              commit()
            }
          }.otherwise {
            commit()
          }
        }.otherwise { // Modify hit
          assert(targetOps === L1DCPort.L1Req.modify)
          val coreWidth = if(opts.CORE_COUNT != 1) { log2Ceil(opts.CORE_COUNT) } else { 1 }
          val writtenDir = L2DirEntry.withAddr(opts, targetAddr).editState(target(coreWidth-1, 0), L2DirState.modified)

          writeDir(
            pipeHitIdx,
            targetAddr,
            writtenDir
          )
        }

        nstate := L2MainState.idle
        step()
      }
    }
  }

  // Refiller, only deals with AR channel
  val axiGrpNum = opts.LINE_WIDTH * 8 / axi.DATA_WIDTH

  val reqTarget = RegInit(0.U(log2Ceil(opts.CORE_COUNT * 3).W))
  val rawReqAddr = missesAddr(reqTarget)

  val reqAddr = rawReqAddr(opts.ADDR_WIDTH-1, OFFSET_LENGTH) ## 0.U(OFFSET_LENGTH.W)
  assume(reqAddr.getWidth == opts.ADDR_WIDTH)

  when(reqTarget < (opts.CORE_COUNT * 2).U) {
    when(sent(reqTarget) =/= misses(reqTarget)) {
      when(misses(reqTarget)) {
        assert(rawReqAddr === addrs(reqTarget))

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
        axi.ARSIZE := AXI.Constants.Size.from(axi.DATA_WIDTH / 8).U

        axi.ARVALID := true.B
        when(axi.ARREADY) {
          sent(reqTarget) := true.B
          stepRefiller(reqTarget)
        }
      } .otherwise {
        sent(reqTarget) := false.B
        stepRefiller(reqTarget)
      }
    }.otherwise {
      // Step
      stepRefiller(reqTarget)
    }
  }.otherwise { // Is uncached
    val id = reqTarget - (opts.CORE_COUNT * 2).U
    when(!directs(id).read || ucSent(reqTarget)) {
      stepRefiller(reqTarget)

      /**
       * MMIO are handled in the write-back state machine
       * because they are faster, so they can be handled synchronizely
       */
    }.elsewhen(isMMIO(directs(id).addr)) {
      stepRefiller(reqTarget)
    }.otherwise {
      axi.ARADDR := directs(id).addr // This may be unaligned
      axi.ARBURST := AXI.Constants.Burst.INCR.U
      axi.ARID := reqTarget
      axi.ARLEN := 0.U
      axi.ARSIZE := DCWriteLen.toAXISize(directs(id).len)

      assume(axi.DATA_WIDTH == opts.XLEN)

      axi.ARVALID := true.B
      when(axi.ARREADY) {
        ucSent(reqTarget) := true.B
        stepRefiller(reqTarget)
      }
    }
  }

  // Receiver, deals with R channel, and victimize
  val bufptrs = RegInit(VecInit(Seq.fill(opts.CORE_COUNT * 2)(
    0.U(log2Ceil(opts.LINE_WIDTH * 8 / axi.DATA_WIDTH).W)
  )))

  when(axi.RVALID) {
    // TODO: handle RRESP
    axi.RREADY := true.B

    when(axi.RID < (opts.CORE_COUNT * 2).U) {
      val ptr = Wire(UInt())
      when(sent(axi.RID)) {
        // Not on the same cycle of AR channel
        ptr := bufptrs(axi.RID)
      }.otherwise {
        // Same cycle on read
        ptr := 0.U
      }

      bufs(axi.RID)(ptr) := axi.RDATA
      bufptrs(axi.RID) := ptr +% 1.U

      when(axi.RLAST) {
        refilled(axi.RID) := true.B
      }
    }.otherwise {
      assert(axi.RLAST)
      val id = axi.RID - (opts.CORE_COUNT * 2).U
      directs(id).rdata := axi.RDATA
      directs(id).stall := false.B
      ucSent(axi.RID) := false.B
    }
  }

  // Write-back...er? Handles AXI AW/W/B
  // Also handles memory mapped reads (CLINT, PLIC, etc...)
  val wbPtr = RegInit(0.U(log2Ceil(axiGrpNum).W))
  val wbDataView = wbFifo(wbFifoHead).data.asTypeOf(Vec(axiGrpNum, UInt(axi.DATA_WIDTH.W)))

  val ucWalkPtr = RegInit(0.U((if(opts.CORE_COUNT == 1) { 1 } else { log2Ceil(opts.CORE_COUNT) }).W))

  object UCSendStage extends ChiselEnum {
    val idle, req, data, resp, mmioReq, mmioResp = Value
  }
  val ucSendStage = RegInit(UCSendStage.idle)

  switch(wbState) {
    is(L2WBState.idle) {
      when(!wbFifoEmpty) {
        // axi.AWID omitted
        axi.AWADDR := wbFifo(wbFifoHead).lineaddr ## 0.U(OFFSET_LENGTH.W)
        axi.AWBURST := AXI.Constants.Burst.INCR.U
        // axi.AWCACHE := omitted
        axi.AWLEN := (axiGrpNum - 1).U
        // axi.AWPROT := omitted
        // axi.AWQOS := omitted
        // axi.AWREGION := omitted
        axi.AWSIZE := AXI.Constants.Size.from(axi.DATA_WIDTH / 8).U
        axi.AWVALID := true.B

        when(axi.AWREADY) {
          wbPtr := 0.U
          wbState := L2WBState.writing
        }
      }.elsewhen(VecInit(directs.map(d => d.write || d.read && isMMIO(d.addr))).asUInt.orR()) {
        assert(ucWalkPtr === 0.U)
        assert(ucSendStage === UCSendStage.idle)
        wbState := L2WBState.ucWalk
      }
    }

    is(L2WBState.writing) {
      axi.WDATA := wbDataView(wbPtr)
      axi.WSTRB := ((1 << (axi.DATA_WIDTH / 8))-1).U
      axi.WVALID := true.B

      val isLast = wbPtr === (axiGrpNum-1).U
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
        wbFifo(wbFifoHead).valid := false.B
        wbState := L2WBState.idle
      }
    }

    is(L2WBState.ucWalk) {
      val pipeUCAddr = Reg(UInt(opts.ADDR_WIDTH.W))
      val pipeUCData = Reg(UInt(opts.XLEN.W))
      val pipeUCLen = Reg(DCWriteLen())

      val pipeMMIOWrite = Reg(Bool())
      val pipeMMIOMap = Reg(UInt(opts.MMIO.size.W))

      switch(ucSendStage) {
        is(UCSendStage.idle) {
          val mmioMap = getMMIOMap(directs(ucWalkPtr).addr)
          val gotoMMIO = mmioMap.orR && (
            directs(ucWalkPtr).read || directs(ucWalkPtr).write
          )
          assert(PopCount(mmioMap) <= 1.U)

          pipeMMIOWrite := directs(ucWalkPtr).write
          pipeMMIOMap := mmioMap
          pipeUCAddr := directs(ucWalkPtr).addr
          pipeUCLen := directs(ucWalkPtr).len
          pipeUCData := directs(ucWalkPtr).wdata

          when(gotoMMIO) {
            ucSendStage := UCSendStage.mmioReq
          }.elsewhen(directs(ucWalkPtr).write) {
            ucSendStage := UCSendStage.req
          }.otherwise {
            when(ucWalkPtr === (opts.CORE_COUNT-1).U) {
              wbState := L2WBState.idle
              ucWalkPtr := 0.U
            }.otherwise {
              ucWalkPtr := ucWalkPtr +% 1.U
            }
          }
        }

        is(UCSendStage.req) {
          assert(directs(ucWalkPtr).write)
          axi.AWADDR := pipeUCAddr
          axi.AWBURST := AXI.Constants.Burst.INCR.U
          axi.AWLEN := 0.U
          axi.AWSIZE := DCWriteLen.toAXISize(pipeUCLen)

          axi.AWVALID := true.B

          when(axi.AWREADY) {
            ucSendStage := UCSendStage.data
          }
        }

        is(UCSendStage.data) {
          axi.WDATA := pipeUCData
          axi.WSTRB := (-1).S((axi.DATA_WIDTH / 8).W).asUInt // Write all stuff
          axi.WLAST := true.B
          axi.WVALID := true.B
          when(axi.WREADY) {
            ucSendStage := UCSendStage.resp
          }
        }

        is(UCSendStage.resp) {
          axi.BREADY := true.B
          when(axi.BVALID) {
            ucSendStage := UCSendStage.idle

            when(ucWalkPtr === (opts.CORE_COUNT-1).U) {
              wbState := L2WBState.idle
              ucWalkPtr := 0.U
            }.otherwise {
              ucWalkPtr := ucWalkPtr +% 1.U
            }

            directs(ucWalkPtr).stall := false.B
          }
        }

        is(UCSendStage.mmioReq) { // TODO: change to MMIOReq
          val req = Wire(new MMIOReq(GeneralMMIODef))
          req.op := Mux(pipeMMIOWrite, MMIOReqOp.write, MMIOReqOp.read)
          req.addr := pipeUCAddr
          req.wdata := pipeUCData // TODO: wstrb
          for((m, e) <- mmio.zip(pipeMMIOMap.asBools())) {
            when(e) {
              m.req.enq(req)
            }
          }

          when(VecInit(mmio.map((m) => m.req.fire())).asUInt.orR) {
            ucSendStage := UCSendStage.mmioResp
          }
        }

        is(UCSendStage.mmioResp) {
          val validMap = mmio.map((m) => m.resp.valid)
          assert(PopCount(validMap) <= 1.U)
          assert(PopCount(VecInit(validMap).asUInt | pipeMMIOMap) <= 1.U)

          directs(ucWalkPtr).rdata := Mux1H(pipeMMIOMap, mmio.map((m) => m.resp.bits))

          when(VecInit(validMap).asUInt().orR) {
            directs(ucWalkPtr).stall := false.B

            ucSendStage := UCSendStage.idle

            // TODO: switch to RRArbiter
            when(ucWalkPtr === (opts.CORE_COUNT-1).U) {
              wbState := L2WBState.idle
              ucWalkPtr := 0.U
            }.otherwise {
              ucWalkPtr := ucWalkPtr +% 1.U
            }
          }
        }
      }
    }
  }
}
