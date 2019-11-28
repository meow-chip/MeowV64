package cache

import chisel3._
import chisel3.experimental.ChiselEnum

trait L1Port extends Bundle {
  def getAddr: UInt
  def getReq: L1D$Port.L1Req.Type

  def getStall: Bool
  def getRdata: UInt
  def getWdata: UInt
}

trait L1Opts {
  val XLEN: Int
  val ADDR_WIDTH: Int

  val TRANSFER_SIZE: Int

  val LINE_WIDTH: Int
  val L1_SIZE: Int
}

/**
 * I$ -> L2
 * 
 * I$ doesn't enforce cache coherence restrictions, so we don't have coherence protocol-related wires
 * Also, I$ doesn't have write channel, so we don't have uplink data wires
 */
class L1I$Port(val opts: L1Opts) extends Bundle with L1Port {
  val read = Output(Bool())
  val addr = Output(UInt(opts.ADDR_WIDTH.W))

  val stall = Input(Bool())
  val data = Input(UInt(opts.TRANSFER_SIZE.W))

  override def getAddr: UInt = addr
  override def getReq = {
    val result = Wire(L1D$Port.L1Req())
    when(read) {
      result := L1D$Port.L1Req.read
    }.otherwise {
      result := L1D$Port.L1Req.idle
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

/**
 * D$ -> L2
 * 
 * We define L2 as the master device, so L1 -> L2 is uplink, and vice-versa
 * 
 * Downlink requests always have higher precedence than uplink requests.
 * However, if a response of an uplink request is going on, it's guaranteed to not be interrupted
 * 
 * In reality, L2 cache operates in an serial manner. No concurrent request may be processed at the same time,
 * hence the guarantee kept
 * 
 * The only exceptions is a read with L2 miss. If that's the case, then no other cache should have the same line,
 * so no additional requests sent to other caches.
 * 
 * Write with L2 miss is an no-op:
 * L1 should enforce the write-allocate policy. A read must be issued if the written line is missed
 * L2 should enforce that all valid lines in L1 is also valid in L2
 */
class L1D$Port(val opts: L1Opts) extends Bundle with L1Port {
  // L1 -> L2 request
  val l1req = Output(L1D$Port.L1Req())
  val l1addr = Output(UInt(opts.ADDR_WIDTH.W))
  val l1stall = Input(Bool())

  // L1 <- L2 request
  val l2req = Input(L1D$Port.L2Req())
  val l2addr = Input(UInt(opts.ADDR_WIDTH.W))
  val l2stall = Output(UInt(opts.ADDR_WIDTH.W))

  // Data bus
  val wdata = Output(UInt(opts.TRANSFER_SIZE.W))
  val rdata = Input(UInt(opts.TRANSFER_SIZE.W))

  override def getAddr: UInt = l1addr
  override def getReq = l1req
  override def getStall: Bool = l1stall
  override def getRdata: UInt = rdata
  override def getWdata: UInt = wdata
}

object L1D$Port {
  /**
   * Uplink requests
   * 
   * - read: request to read one cache line
   * - readWrite: request to write allocate one cache line
   * - modify: request to invalidate all other out-standing cache duplicates
   * - writeback: request to writeback a line
   */
  object L1Req extends ChiselEnum {
    // TODO: do we include inval here? is it worth it?
    val idle, read, readWrite, modify, writeback = Value
  }

  /**
   * Downlink requests
   * 
   * - flush: request to write-back one cache line. This should generate a writeback event,
   *     overriding the pending event on the port
   * - inval: request to invalidate one cache line
   *     If the invalidated cache line is also a target of a pending write in write queue,
   *     especially the head of the write queue, L1 should fetch (write-allocate) the line again before
   *     sending an modify request
   */
  object L2Req extends ChiselEnum {
    val idle, flush, inval = Value
  }
}
