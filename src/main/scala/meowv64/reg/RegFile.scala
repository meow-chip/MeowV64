package meowv64.reg

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
class RegFile(XLEN: Int = 64, COUNT: Int = 32, READ_COUNT: Int = 2, WRITE_COUNT: Int = 1) extends Module {
  val io = IO(new Bundle {
    val reads = Vec(READ_COUNT, Flipped(new RegReader(XLEN, COUNT)))
    val writes = Flipped(Vec(WRITE_COUNT, new RegWriter(XLEN, COUNT)))
  })

  val regs = RegInit(VecInit(List.fill(COUNT)(0).map(_.U(XLEN.W))))

  for(read <- io.reads) {
    when(read.addr === 0.U) {
      read.data := 0.U
    }.otherwise {
      read.data := regs(read.addr)
    }
  }

  for(write <- io.writes) {
    when(write.addr =/= 0.U) {
      regs(write.addr) := write.data
    }
  }

  /*
  printf("RegFile status: \n")
  printf("================\n")
  for(i <- (0 until COUNT)) {
    val prefix = ("x" + i).reverse.padTo(3, ' ').reverse
    printf(p" | $prefix: 0x${Hexadecimal(regs(i.U))}")
    if(i % 4 == 3) printf(" |\n")
  }
  */
}
