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
class Passthrough extends Module {
  val io = IO(new ICachePort)

  val sIDLE :: sREQUEST :: sRECV :: Nil = Enum(3)
  val state = RegInit(sIDLE)

  val fetchingAddr = RegInit(0.U(48.W))
  val cnt = RegInit(0.U(log2Ceil(64 / 8).W))
  val result = RegInit(VecInit((0 until 8).map(_ => { 0.U(8.W) })))

  io.axi := 0.U
  io.axi.ARLEN := 8.U
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
        when(cnt === 7.U) {
          state := sIDLE
        }
      }
    }
  }
}