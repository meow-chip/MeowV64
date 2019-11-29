package core

import chisel3._
import chisel3.util._

class CounterPort extends Bundle {
  val op = Output(CSROp())
  val valid = Output(Bool())
  val rdata = Input(UInt(64.W)) // width of machine counters is the same on RV32/64
  val wdata = Output(UInt(64.W))
}

class Counter(XLEN: Int) extends Module {
  val io = IO(Flipped(new CounterPort))

  val counter = RegInit(0.U(64.W))
  val wdata = Wire(UInt(XLEN.W))
  io.rdata := counter

  wdata := io.rdata + 1.U

  when(io.valid) {
    switch(io.op) {
      is(CSROp.rw) {
        wdata := io.wdata
      }
      is(CSROp.rs) {
        when(io.wdata =/= 0.U(XLEN.W)) {
          wdata := io.rdata | io.wdata
        }
      }
      is(CSROp.rc) {
        when(io.wdata =/= 0.U(XLEN.W)) {
          wdata := io.rdata & (~io.wdata)
        }
      }
    }
  }

  counter := wdata
}
