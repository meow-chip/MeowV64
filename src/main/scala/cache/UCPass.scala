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
class UCPass(val opts: L1DOpts) extends MultiIOModule {
  // Ports
  val toCPU = IO(Flipped(new L1UCPort(opts)))

  val axi = IO(new AXI(opts.XLEN))

  axi <> 0.U.asTypeOf(axi)

  // Reader
  object RState extends ChiselEnum {
    val idle, req, data = Value;
  }

  val rstate = RegInit(RState.idle)
  val nrstate = Wire(RState())
  rstate := nrstate
  nrstate := rstate


  toCPU.rdata := axi.RDATA

  switch(rstate) {
    is(RState.idle) {
      when(toCPU.read) {
        nrstate := RState.req
      }
    }

    is(RState.req) {
      axi.ARADDR := toCPU.addr
      axi.ARVALID := true.B
      axi.ARSIZE := AXI.Constants.Size.S8.U
      axi.ARLEN := 0.U
      axi.ARBURST := AXI.Constants.Burst.INCR.U

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

  // Writer
  object WState extends ChiselEnum {
    val idle, req, data, resp = Value;
  }

  val wstate = RegInit(WState.idle)
  val nwstate = Wire(WState())
  wstate := nwstate
  nwstate := wstate

  switch(wstate) {
    is(WState.idle) {
      when(toCPU.write) {
        nwstate := WState.req
      }
    }

    is(WState.req) {
      axi.AWADDR := toCPU.addr
      axi.AWVALID := true.B
      axi.AWSIZE := AXI.Constants.Size.S8.U
      axi.AWLEN := 0.U
      axi.AWBURST := AXI.Constants.Burst.INCR.U

      when(axi.AWREADY) {
        nwstate := WState.data
      }
    }

    is(WState.data) {
      axi.WDATA := toCPU.wdata
      axi.WSTRB := toCPU.wstrb
      axi.WLAST := true.B
      axi.WVALID := true.B
      when(axi.WVALID) {
        nwstate := WState.resp
      }
    }

    is(WState.resp) {
      axi.BREADY := true.B
      when(axi.BVALID) {
        nwstate := WState.idle
      }
    }
  }

  toCPU.stall := nrstate =/= RState.idle || nwstate =/= WState.idle
}
