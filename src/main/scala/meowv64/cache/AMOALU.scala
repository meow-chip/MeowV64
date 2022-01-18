package meowv64.cache

import chisel3._
import chisel3.util._
import chisel3.util.log2Ceil

/** Computes atomic operations
  */
class AMOALU(val opts: L1DOpts) extends Module {
  val io = IO(new Bundle {
    val op = Input(DCWriteOp()) // write is treated as idle
    val rdata = Input(UInt(opts.XLEN.W))
    val wdata = Input(UInt(opts.XLEN.W))

    val offset = Input(
      UInt(log2Ceil(opts.XLEN / 8).W)
    ) // If op =/= write or idle, then length must be W or D
    val length = Input(DCWriteLen())

    val rsliced = Output(
      UInt(opts.XLEN.W)
    ) // Only valid if length is W or D. This is for AMO
    val muxed = Output(UInt(opts.XLEN.W))
  })

  val rextended = Wire(SInt(opts.XLEN.W))
  rextended := io.rdata.asSInt()

  val rconverted = rextended.asUInt()
  val rraw = Wire(UInt(opts.XLEN.W))
  io.rsliced := rconverted

  rraw := io.rdata
  // clip wdata to 32 bits if necessary
  val wdata = Wire(UInt(opts.XLEN.W))
  wdata := io.wdata
  when(io.length === DCWriteLen.W) {
    wdata := io.wdata(31, 0)
    when(io.offset.head(1) === 0.U) {
      rextended := io.rdata(31, 0).asSInt()
      rraw := io.rdata(31, 0)
    }.otherwise {
      rextended := io.rdata(63, 32).asSInt()
      rraw := io.rdata(63, 32)
    }
  }

  val filtered = Wire(UInt(opts.XLEN.W))
  filtered := DontCare
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
      filtered := rextended.max(wdata.asSInt()).asUInt()
    }

    is(DCWriteOp.maxu) {
      filtered := rraw.max(io.wdata)
    }

    is(DCWriteOp.min) {
      filtered := rextended.min(wdata.asSInt()).asUInt()
    }

    is(DCWriteOp.minu) {
      filtered := rraw.min(io.wdata)
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
