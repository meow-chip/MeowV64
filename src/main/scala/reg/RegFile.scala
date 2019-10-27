package reg

import chisel3._
import chisel3.util.log2Ceil

class RegReader(val XLEN: Int = 64, val COUNT: Int = 32) extends Bundle {
  val addr = Output(UInt(log2Ceil(COUNT).W))
  val data = Input(UInt(XLEN.W))
}

class RegWriter(val XLEN: Int = 64, val COUNT: Int = 32) extends Bundle {
  val addr = Output(UInt(log2Ceil(COUNT).W))
  val data = Output(UInt(XLEN.W))
}

// Standard Registers
// TODO: support multi port
class RegFile(XLEN: Int = 64, COUNT: Int = 32, READ_COUNT: Int = 2) extends Module {
  val io = IO(new Bundle {
    val reads = Vec(READ_COUNT, Flipped(new RegReader(XLEN, COUNT)))
    val write = Flipped(new RegWriter(XLEN, COUNT))
  })

  val regs = RegInit(VecInit(List.fill(COUNT)(0).map(_.U(XLEN.W))))

  for(i <- (0 until READ_COUNT)) {
    when(io.reads(i).addr === 0.U) {
      io.reads(i).data := 0.U
    }.elsewhen(io.reads(i).addr === io.write.addr) {
      io.reads(i).data := io.write.data
    }.otherwise {
      io.reads(i).data := regs(io.reads(i).addr)
    }
  }

  when(io.write.addr =/= 0.U) {
    regs(io.write.addr) := io.write.data
  }

  printf("RegFile status: \n")
  printf("================\n")
  for(i <- (0 until COUNT)) {
    printf(p"x$i: ${regs(i.U)}\n")
    if(i % 4 == 3) printf("\n")
  }
}
