package reg

import chisel3._
import chisel3.util.log2Ceil

class RegReader(XLEN: Int = 64, COUNT: Int = 32) extends Bundle {
  val addr = Output(UInt(log2Ceil(COUNT).W))
  val data = Input(UInt(XLEN.W))
}

class RegWriter(XLEN: Int = 64, COUNT: Int = 32) extends Bundle {
  val addr = Output(UInt(log2Ceil(COUNT).W))
  val data = Output(UInt(XLEN.W))
}

// Standard Registers
// TODO: support multi port
class RegFile(XLEN: Int = 64, COUNT: Int = 32) extends Module {
  val io = IO(new Bundle {
    val read = Flipped(new RegReader(XLEN, COUNT))
    val write = Flipped(new RegWriter(XLEN, COUNT))
  })

  val regs = RegInit(VecInit(List.fill(COUNT)(0).map(_.U(XLEN.W))))

  when(io.read.addr === 0.U) {
    io.read.data := 0.U
  }.elsewhen(io.read.addr === io.write.addr) {
    io.read.data := io.write.data
  }.otherwise {
    io.read.data := regs(io.read.addr)
  }

  when(io.write.addr =/= 0.U) {
    regs(io.write.addr) := io.write.data
  }
}