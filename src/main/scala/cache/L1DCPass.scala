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
class L1DCPass(val opts: L1DOpts) extends MultiIOModule {
  // Ports
  val r = IO(Flipped(new DCReader(opts)))
  val w = IO(Flipped(new DCWriter(opts)))
  val fs = IO(Flipped(new DCFenceStatus(opts)))

  val axi = IO(new AXI(opts.XLEN))

  axi <> 0.U.asTypeOf(axi)

  fs.wbufClear := true.B

  // Reader
  object RState extends ChiselEnum {
    val idle, req, data = Value;
  }

  val pipeRead = RegInit(false.B)
  val pipeAddr = Reg(UInt(opts.ADDR_WIDTH.W))

  when(!r.stall) {
    pipeRead := r.read
    pipeAddr := r.addr
  }

  val rstate = RegInit(RState.idle)
  val nrstate = Wire(RState())
  rstate := nrstate
  nrstate := rstate
  r.stall := nrstate =/= RState.idle

  r.data := axi.RDATA

  switch(rstate) {
    is(RState.idle) {
      when(pipeRead) {
        nrstate := RState.req
      }
    }

    is(RState.req) {
      axi.ARADDR := pipeAddr
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
  w.stall := nwstate =/= WState.idle

  switch(wstate) {
    is(WState.idle) {
      when(w.write) {
        nwstate := WState.req
      }
    }

    is(WState.req) {
      axi.AWADDR := w.addr
      axi.AWVALID := true.B
      axi.AWSIZE := AXI.Constants.Size.S8.U
      axi.AWLEN := 0.U
      axi.AWBURST := AXI.Constants.Burst.INCR.U

      when(axi.AWREADY) {
        nwstate := WState.data
      }
    }

    is(WState.data) {
      axi.WDATA := w.data
      axi.WSTRB := w.be
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
}
