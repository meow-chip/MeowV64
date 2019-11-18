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
  val L2_LINE_WIDTH: Int // In bytes
  val L2_SIZE: Int // In bytes
  val L2_ASSOC: Int

  val CORE_COUNT: Int

  // TODO: check is log2
  assume(L2_LINE_WIDTH % L1_LINE_WIDTH == 0)
  assume(L2_SIZE % L2_LINE_WIDTH == 0)
}

object L2DirState extends ChiselEnum {
  val vacant, shared, modified = Value
}

class L2DirEntry(val opts: L2Opts) extends Bundle {
  val TAIL_LENGTH = log2Ceil(opts.L2_SIZE)
  val TAG_LENGTH = opts.ADDR_WIDTH - TAIL_LENGTH

  val valid = Bool()
  val tag = UInt(TAG_LENGTH.W)
  val states = Vec(opts.CORE_COUNT, L2DirState())

  def hit(addr: UInt): Bool = valid && addr(opts.ADDR_WIDTH-1, TAIL_LENGTH) === tag
}

object L2DirEntry {
  def default(opts: L2Opts): L2DirEntry = {
    val w = Wire(new L2DirEntry(opts))
    w := DontCare
    w.valid := false.B

    w
  }
}

class L2Assoc(val opts: L2Opts) {
  val lineCount = opts.L2_SIZE / opts.L2_LINE_WIDTH / opts.L2_ASSOC

  val directory = SyncReadMem(lineCount, new L2DirEntry(opts))
  val store = SyncReadMem(lineCount, Vec(opts.L2_LINE_WIDTH / opts.L1_LINE_WIDTH, UInt((opts.L1_LINE_WIDTH * 8).W)));
}

class L1PortBuffer[T <: L1Port](val port: T, val opts: L2Opts) extends Bundle {
  val buffer = Vec(opts.L1_LINE_WIDTH / opts.TRANSFER_SIZE, UInt((opts.TRANSFER_SIZE * 8).W))
  val cnt = UInt(log2Ceil(opts.L1_LINE_WIDTH / opts.TRANSFER_SIZE).W)

  val missed = Bool()
  val ready = Bool()
  val done = Bool()

  def hitFill(v: UInt) = {
    buffer := v.asTypeOf(buffer)
    missed := false.B
    ready := true.B
    done := false.B
  }

  def miss() = {
    missed := true.B
    ready := false.B
  }
}

object L1PortBuffer {
  def default[T <: L1Port](p: T, o: L2Opts): L1PortBuffer[T] = {
    val w = Wire(new L1PortBuffer(p, o))
    w := 0.U.asTypeOf(w)
    w
  }
}

object L2MainState extends ChiselEnum {
  val reset, idle, mux = Value
}

class L2Cache(val opts: L2Opts) extends MultiIOModule {
  val IGNORE_LENGTH = log2Ceil(opts.L1_LINE_WIDTH)
  val OFFSET_LENGTH = log2Ceil(opts.L2_LINE_WIDTH)
  val INDEX_OFFSET_LENGTH = log2Ceil(opts.L2_SIZE)

  val i$ = IO(Vec(opts.CORE_COUNT, Flipped(new L1I$Port(opts))))
  val d$ = IO(Vec(opts.CORE_COUNT, Flipped(new L1D$Port(opts))))

  val axi = IO(new AXI(opts.XLEN, opts.ADDR_WIDTH))

  val assocs = for(_ <- (0 until opts.L2_ASSOC)) yield new L2Assoc(opts)

  val i$bufs = VecInit(
    for(i <- (0 until opts.CORE_COUNT)) yield RegInit(L1PortBuffer.default(i$(i), opts))
  )
  val d$bufs = VecInit(
    for(i <- (0 until opts.CORE_COUNT)) yield RegInit(L1PortBuffer.default(d$(i), opts))
  )

  val target = RegInit(0.U(log2Ceil(opts.CORE_COUNT).W))
  val targetingI$ = RegInit(true.B)

  val rstCnt = RegInit(0.U)
  val rstDir = L2DirEntry.default(opts)

  val state = RegInit(L2MainState.reset)
  val nstate = Wire(L2MainState())

  nstate := state
  state := nstate

  when(nstate === L2MainState.idle) {
    when(target != (opts.CORE_COUNT-1).U) {
      target := target + 1.U
    }.otherwise {
      target := 0.U
      targetingI$ := !targetingI$
    }
  }

  val targetAddr = Wire(UInt(opts.ADDR_WIDTH.W))
  when(targetingI$) {
    targetAddr := i$(target).addr
  }.otherwise {
    targetAddr := i$(target).addr
  }
  val targetIndex = targetAddr(INDEX_OFFSET_LENGTH-1, OFFSET_LENGTH)
  val queries = assocs.map(a => (a.directory.read(targetIndex), a.store.read(targetIndex)))

  switch(state) {
    is(L2MainState.reset) {
      for(assoc <- assocs) {
        assoc.directory.write(rstCnt, rstDir)
      }

      rstCnt := rstCnt + 1.U

      when(rstCnt === (opts.L2_SIZE / opts.L2_LINE_WIDTH / opts.L2_ASSOC).U) {
        nstate := L2MainState.idle
      }
    }

    is(L2MainState.idle) {
      // Wait for query result
      // TODO: pipelining
      // TODO: check refill
      nstate := L2MainState.mux
    }

    is(L2MainState.mux) {
      val (hit, data) = queries.foldRight((false.B, 0.U))((cur, acc) => {
        val hit = cur._1.hit(targetAddr)
        val line = Wire(UInt())
        when(hit) {
          line := cur._2
        }.otherwise {
          line := 0.U
        }

        (acc._1 || hit, acc._2 | line)
      })

      val dataView = Wire(Vec(opts.L2_LINE_WIDTH / opts.L1_LINE_WIDTH, UInt((opts.L1_LINE_WIDTH * 8).W)))

      when(hit) {
        nstate := L2MainState.idle

        when(targetingI$) {
          i$bufs(target).hitFill(dataView(targetAddr(OFFSET_LENGTH-1, IGNORE_LENGTH)))
        }.otherwise {
          d$bufs(target).hitFill(dataView(targetAddr(OFFSET_LENGTH-1, IGNORE_LENGTH)))
        }
      }.otherwise {
        when(targetingI$) {
          i$bufs(target).miss()
        }.otherwise {
          d$bufs(target).miss()
        }
      }
    }
  }
}
