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
  val io = IO(new ICachePort(ADDR_WIDTH, DATA_LEN))

  val sIDLE :: sREQUEST :: sRECV :: Nil = Enum(3)
  val state = RegInit(sIDLE)

  val fetchingAddr = RegInit(0.U(ADDR_WIDTH.W))
  val cnt = RegInit(0.U(log2Ceil(DATA_LEN / 8).W))
  val result = RegInit(VecInit((0 until DATA_LEN/8).map(_ => { 0.U(8.W) })))

  io.axi := 0.U
  io.axi.ARLEN := (DATA_LEN / 8).U
  io.axi.ARSIZE := AXI.Constants.Size.S8.U
  io.axi.ARBURST := AXI.Constants.Burst.INCR.U

  io.stall := state =/= sIDLE
  io.data := result.asUInt

  switch(state) {
    is(sIDLE) {
      when(!io.pause && io.read) {
        fetchingAddr := io.addr
        state := sREQUEST
        cnt := 0.U
      }
    }

    is(sREQUEST) {
      io.axi.ARADDR := fetchingAddr
      io.axi.ARVALID := true.B

      when(io.axi.ARREADY) {
        state := sRECV
      }
    }

    is(sRECV) {
      io.axi.RREADY := true.B
      when(io.axi.RVALID) {
        result(cnt) := io.axi.RDATA
        cnt := cnt + 1.U
        when(cnt === (DATA_LEN-1).U) {
          state := sIDLE
        }
      }
    }
  }
}