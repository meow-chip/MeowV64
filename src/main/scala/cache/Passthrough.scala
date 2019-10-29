package cache

import chisel3._
import chisel3.util._
import data.AXI

/**
 * A basic passthrough cache
 * which is *NOT* a cache
 * 
 * TODO: add pipeline
 * TODO: add flush
 */
class Passthrough(ADDR_WIDTH: Int, DATA_LEN: Int) extends Module {
  val io = IO(new DCachePort(ADDR_WIDTH, DATA_LEN))

  val sIDLE :: sREQUEST_READ :: sRECV :: sREQUEST_WRITE :: sSEND :: sRESP :: Nil = Enum(6)
  val state = RegInit(sIDLE)

  val workingAddr = RegInit(0.U(ADDR_WIDTH.W))
  val workingData = Reg(Vec(DATA_LEN/8, UInt(8.W)))
  val workingBE = RegInit(0.U((DATA_LEN/8).W))

  val cnt = RegInit(0.U(log2Ceil(DATA_LEN / 8).W))
  val result = RegInit(VecInit((0 until DATA_LEN/8).map(_ => { 0.U(8.W) })))
  val pipeRead = RegInit(false.B)
  val pipeWrite = RegInit(false.B)

  io.axi <> 0.U.asTypeOf(io.axi)
  io.axi.ARLEN := (DATA_LEN / 8).U
  io.axi.ARSIZE := AXI.Constants.Size.S8.U
  io.axi.ARBURST := AXI.Constants.Burst.INCR.U

  io.axi.AWLEN := (DATA_LEN / 8).U
  io.axi.AWSIZE := AXI.Constants.Size.S8.U
  io.axi.AWBURST := AXI.Constants.Burst.INCR.U

  io.stall := state =/= sIDLE
  io.rdata := result.asUInt
  io.vacant := !(pipeRead || pipeWrite)

  /*
  printf("Cache state:\n================\n")
  printf(p"State: ${state}\n\n")
  */

  when(!io.pause && !io.stall) {
    pipeRead := io.read
    pipeWrite := io.write
  }

  switch(state) {
    is(sIDLE) {
      when(!io.pause) {
        workingAddr := io.addr
        workingData := io.wdata.asTypeOf(workingData)
        workingBE := io.be
        cnt := 0.U

        when(io.read) {
          state := sREQUEST_READ
        }.elsewhen(io.write) {
          state := sREQUEST_WRITE
        }
      }
    }

    is(sREQUEST_READ) {
      io.axi.ARADDR := workingAddr
      io.axi.ARVALID := true.B

      when(io.axi.ARREADY) {
        state := sRECV
      }
    }

    is(sREQUEST_WRITE) {
      io.axi.AWADDR := workingAddr
      io.axi.AWVALID := true.B

      when(io.axi.AWREADY) {
        state := sSEND
      }
    }

    is(sRECV) {
      io.axi.RREADY := true.B
      when(io.axi.RVALID) {
        result(cnt) := io.axi.RDATA
        cnt := cnt + 1.U
        when(io.axi.RLAST) {
          state := sIDLE
        }
      }
    }

    is(sSEND) {
      io.axi.WVALID := true.B
      io.axi.WDATA := workingData(cnt)
      io.axi.WSTRB := workingBE(cnt).asUInt

      io.axi.WLAST := cnt === (DATA_LEN/8 - 1).U

      when(io.axi.WREADY) {
        when(io.axi.WLAST) {
          state := sRESP
        }.otherwise {
          cnt := cnt + 1.U;
        }
      }
    }

    is(sRESP) {
      io.axi.BREADY := true.B
      when(io.axi.BVALID) {
        state := sIDLE
      }
    }
  }
}
