package cache

import chisel3._
import _root_.data._
import chisel3.util.log2Ceil
import chisel3.experimental.ChiselEnum
import chisel3.util._
import Chisel.experimental.chiselName
import cache.L1DCPort.L2Req
import cache.L1DCPort.L1Req

@chiselName
class L1ICPass(val opts: L1Opts) extends MultiIOModule {
  val toCPU = IO(new ICPort(opts))

  val axi = IO(new AXI(opts.XLEN))

  axi := 0.U.asTypeOf(axi)

  // Silently ignores toCPU.rst

  // Reader
  object RState extends ChiselEnum {
    val idle, req, data = Value;
  }

  val rstate = RegInit(RState.idle)
  val nrstate = Wire(RState())
  rstate := nrstate
  nrstate := rstate
  toCPU.stall := nrstate =/= RState.idle

  val aligned = toCPU.addr(opts.ADDR_WIDTH-1, 3)
  val offset = toCPU.addr(2)

  toCPU.data := Mux(offset, axi.RDATA(63, 32), axi.RDATA(31, 0))

  switch(rstate) {
    is(RState.idle) {
      when(toCPU.read) {
        nrstate := RState.req
      }
    }

    is(RState.req) {
      axi.ARADDR := aligned
      axi.ARVALID := true.B
      axi.ARSIZE := AXI.Constants.Size.S8.U
      axi.ARLEN := 0.U

      when(axi.ARREADY) {
        nrstate := RState.data
      }
    }

    is(RState.data) {
      axi.RREADY := true.B
      when(axi.RVALID) {
        nrstate := RState.idle
      }
    }
  }
}
