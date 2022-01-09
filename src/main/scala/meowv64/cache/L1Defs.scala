package meowv64.cache

import chisel3._
import chisel3.experimental.ChiselEnum

/** Cache definations and interfaces
  *
  * Unless explicitly stated, size/width numbers are in bits
  */

/** Trait of L1 cache's external interface
  *
  * This is used in L2 to generate polymorphic interfaces L1 can never actively
  * stalls L2
  */
trait L1Port extends Bundle {
  // Address
  def getAddr: UInt
  // L1 -> L2 request
  def getReq: L1DCPort.L1Req.Type

  // L2 -> L1 stall
  def getStall: Bool
  // L2 -> L1 read data
  def getRdata: UInt
  // L1 -> L2 write data
  def getWdata: UInt
}

trait L1Opts {
  // Word width
  val XLEN: Int
  // Address width
  val ADDR_WIDTH: Int

  /** L1 <-> L2 transfer size in bits.
    *
    * Currently, it's only possible that TRANSFER_WIDTH = L1 LINE_BYTES = L2
    * LINE_BYTES
    */
  val TRANSFER_WIDTH: Int

  /**
    * Line width in bytes
    */
  val LINE_BYTES: Int
  /**
    * Cache size in bytes
    */
  val SIZE_BYTES: Int
  val ASSOC: Int

  // TODO: check is log2
  assume(SIZE_BYTES % LINE_BYTES == 0)
}

trait L1DOpts extends L1Opts {
  // Write buffer depth in L1DC
  val WRITE_BUF_DEPTH: Int
}

/** I$ -> L2
  *
  * I$ doesn't enforce cache coherence restrictions, so we don't have coherence
  * protocol-related wires Also, I$ doesn't have write channel, so we don't have
  * uplink data wires
  */
class L1ICPort(val opts: L1Opts) extends Bundle with L1Port {
  val read = Output(Bool())
  val addr = Output(UInt(opts.ADDR_WIDTH.W))

  val stall = Input(Bool())
  val data = Input(UInt((opts.LINE_BYTES * 8).W))

  override def getAddr: UInt = addr
  override def getReq = {
    val result = Wire(L1DCPort.L1Req())
    when(read) {
      result := L1DCPort.L1Req.read
    }.otherwise {
      result := L1DCPort.L1Req.idle
    }

    result
  }

  override def getStall: Bool = stall
  override def getRdata = data
  override def getWdata = {
    val ret = UInt()
    ret <> DontCare
    ret
  }
}

object L1ICPort {
  def empty(opts: L1Opts): L1ICPort = {
    val port = Wire(Flipped(new L1ICPort(opts)))
    port := DontCare
    port.read := false.B

    port
  }
}

/** Uncached access
  */
class L1UCPort(val opts: L1Opts) extends Bundle {
  val read = Output(Bool())
  val write = Output(Bool())
  val addr = Output(UInt(opts.ADDR_WIDTH.W))
  val len = Output(DCWriteLen())

  val stall = Input(Bool())

  val wdata = Output(UInt(opts.XLEN.W))
  val rdata = Input(UInt(opts.XLEN.W))
}

object L1UCPort {
  def empty(opts: L1Opts): L1UCPort = {
    val port = Wire(Flipped(new L1UCPort(opts)))
    port := DontCare
    port.read := false.B
    port.write := false.B

    port
  }
}

/** D$ -> L2
  *
  * We define L2 as the master device, so L1 -> L2 is uplink, and vice-versa
  *
  * Downlink requests always have higher precedence than uplink requests.
  * However, if a response of an uplink request is going on, it's guaranteed to
  * not be interrupted
  *
  * In reality, L2 cache operates in an serial manner. No concurrent request may
  * be processed at the same time, hence the guarantee kept
  *
  * The only exceptions is a read with L2 miss. If that's the case, then no
  * other cache should have the same line, so no additional requests sent to
  * other caches.
  *
  * Write with L2 miss is an no-op: L1 should enforce the write-allocate policy.
  * A read must be issued if the written line is missed L2 should enforce that
  * all valid lines in L1 is also valid in L2
  */
class L1DCPort(val opts: L1Opts) extends Bundle with L1Port {
  // L1 -> L2 request
  val l1req = Output(L1DCPort.L1Req())
  val l1addr = Output(UInt(opts.ADDR_WIDTH.W))
  val l1stall = Input(Bool())

  // L1 <- L2 request
  val l2req = Input(L1DCPort.L2Req())
  val l2addr = Input(UInt(opts.ADDR_WIDTH.W))
  val l2stall = Output(Bool())
  // TODO: add a debug signal to show if L1 really has the entry

  // Data bus
  val wdata = Output(UInt((opts.LINE_BYTES * 8).W))
  val rdata = Input(UInt((opts.LINE_BYTES * 8).W))

  override def getAddr: UInt = l1addr
  override def getReq = l1req
  override def getStall: Bool = l1stall
  override def getRdata: UInt = rdata
  override def getWdata: UInt = wdata
}

object L1DCPort {

  /** Uplink requests
    *
    *   - read: request to read one cache line
    *   - readWrite: request to write allocate one cache line
    *   - modify: request to invalidate all other out-standing cache duplicates
    *   - writeback: request to writeback a line
    */
  object L1Req extends ChiselEnum {
    // TODO: do we include inval here? is it worth it?
    val idle, read, modify, writeback = Value
  }

  /** Downlink requests
    *
    *   - flush: request to write-back one cache line. This should generate a
    *     writeback event, overriding the pending event on the port
    *   - inval: request to invalidate one cache line If the invalidated cache
    *     line is also a target of a pending write in write queue, especially
    *     the head of the write queue, L1 should fetch (write-allocate) the line
    *     again before sending an modify request
    */
  object L2Req extends ChiselEnum {
    val idle, flush, inval = Value
  }

  def empty(opts: L1Opts): L1DCPort = {
    val port = Wire(Flipped(new L1DCPort(opts)))
    port := DontCare
    port.l1addr := 0.U
    port.l1req := L1Req.idle
    port.l2stall := false.B

    port
  }
}
