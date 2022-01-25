package meowv64.reg

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util.BitPat
import chisel3.util.log2Ceil

object RegType extends ChiselEnum {
  val integer, float, vector = Value

  implicit def bitpat(op: RegType.Type): BitPat =
    BitPat(op.litValue.U(getWidth.W))
}

class RegReader(val WIDTH: Int = 64, val COUNT: Int = 32) extends Bundle {
  val addr = Output(UInt(log2Ceil(COUNT).W))
  val data = Input(UInt(WIDTH.W))
}

class RegWriter(val WIDTH: Int = 64, val COUNT: Int = 32) extends Bundle {
  // this is required since float register 0 is not wired to zero
  val valid = Output(Bool())
  val addr = Output(UInt(log2Ceil(COUNT).W))
  val data = Output(UInt(WIDTH.W))
}

// Standard Registers
// TODO: support multi port
class RegFile(
    WIDTH: Int = 64,
    COUNT: Int = 32,
    READ_COUNT: Int = 2,
    WRITE_COUNT: Int = 1,
    FIXED_ZERO: Boolean = true
) extends Module {
  val io = IO(new Bundle {
    val reads = Vec(READ_COUNT, Flipped(new RegReader(WIDTH, COUNT)))
    val writes = Flipped(Vec(WRITE_COUNT, new RegWriter(WIDTH, COUNT)))
  })

  val regs = RegInit(VecInit(List.fill(COUNT)(0).map(_.U(WIDTH.W))))

  for (read <- io.reads) {
    when(read.addr === 0.U && FIXED_ZERO.B) {
      // x0 is hard wired to zero
      read.data := 0.U
    }.otherwise {
      read.data := regs(read.addr)
    }
  }

  for (write <- io.writes) {
    when(write.valid) {
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
