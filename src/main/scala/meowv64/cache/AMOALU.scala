package meowv64.cache

import chisel3._
import chisel3.util._
import chisel3.util.log2Ceil

/** Computes atomic operations
  */
class AMOALU(val opts: L1DOpts) extends Module {
  val io = IO(new Bundle {
    val op = Input(DCWriteOp()) // write is treated as idle

    /** Data read from memory
      */
    val rdata = Input(UInt(opts.XLEN.W))

    /** Data from register
      */
    val wdata = Input(UInt(opts.XLEN.W))

    val offset = Input(
      UInt(log2Ceil(opts.XLEN / 8).W)
    ) // If op =/= write or idle, then length must be W or D
    val length = Input(DCWriteLen())

    // output register
    val rsliced = Output(
      UInt(opts.XLEN.W)
    ) // Only valid if length is W or D. This is for AMO

    /** Data to be written to memory
      */
    val muxed = Output(UInt(opts.XLEN.W))
  })

  /** Memory, sign extended to 64 bits
    */
  val rextended = Wire(SInt(opts.XLEN.W))
  rextended := io.rdata.asSInt()

  val rconverted = rextended.asUInt()

  /** Return register: memory sign extended to 64 bits
    */
  io.rsliced := rconverted

  /** Memory, zero extended to 64 bits
    */
  val rraw = Wire(UInt(opts.XLEN.W))

  rraw := io.rdata

  // clip wdata to 32 bits if necessary
  val wdataSigned = Wire(SInt(opts.XLEN.W))
  val wdataUnsigned = Wire(UInt(opts.XLEN.W))
  wdataUnsigned := io.wdata
  wdataSigned := io.wdata.asSInt()

  when(io.length === DCWriteLen.W) {
    wdataUnsigned := io.wdata(31, 0)
    wdataSigned := io.wdata(31, 0).asSInt()
    when(io.offset.head(1) === 0.U) {
      rextended := io.rdata(31, 0).asSInt()
      rraw := io.rdata(31, 0)
    }.otherwise {
      rextended := io.rdata(63, 32).asSInt()
      rraw := io.rdata(63, 32)
    }
  }

  /** Compute result
    */
  val filtered = Wire(UInt(opts.XLEN.W))
  filtered := 0.U
  switch(io.op) {
    is(DCWriteOp.idle, DCWriteOp.write, DCWriteOp.cond, DCWriteOp.swap) {
      filtered := io.wdata
    }

    is(DCWriteOp.add) {
      filtered := rconverted + io.wdata
    }

    is(DCWriteOp.and) {
      filtered := rconverted & io.wdata
    }

    is(DCWriteOp.or) {
      filtered := rconverted | io.wdata
    }

    is(DCWriteOp.xor) {
      filtered := rconverted ^ io.wdata
    }

    is(DCWriteOp.max) {
      filtered := rextended.max(wdataSigned).asUInt()
    }

    is(DCWriteOp.maxu) {
      filtered := rraw.max(wdataUnsigned)
    }

    is(DCWriteOp.min) {
      filtered := rextended.min(wdataSigned).asUInt()
    }

    is(DCWriteOp.minu) {
      filtered := rraw.min(wdataUnsigned)
    }
  }

  when(io.length === DCWriteLen.D) {
    io.muxed := filtered
  }.elsewhen(io.offset.head(1) === 0.U) {
    io.muxed := io.rdata(63, 32) ## filtered(31, 0)
  }.otherwise {
    io.muxed := filtered(31, 0) ## io.rdata(31, 0)
  }
}
