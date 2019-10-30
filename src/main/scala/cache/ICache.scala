package cache

import chisel3._
import _root_.data._

class ICachePort(val ADDR_WIDTH: Int, val DATA_LEN: Int, val XLEN: Int) extends Bundle {
  val addr = Input(UInt(ADDR_WIDTH.W))
  val read = Input(Bool())

  val axi = new AXI(XLEN)

  val stall = Output(Bool())
  val pause = Input(Bool())
  val flush = Input(Bool()) // Branch missperdict, flushing all running requests

  val data = Output(UInt(DATA_LEN.W)) // Data delay is 1 cycle
  val vacant = Output(Bool())
}

// TODO: Change to xpm_tdpmem
class ICache(ADDR_WIDTH: Int, DATA_LEN: Int, XLEN: Int) extends Module {
  val io = IO(new ICachePort(ADDR_WIDTH, DATA_LEN, XLEN))

  val inner = Module(new Passthrough(ADDR_WIDTH, XLEN))

  val pipeAddr = RegInit(0.U(ADDR_WIDTH.W))

  when(!io.stall && !io.pause) {
    pipeAddr := io.addr
  }

  // TODO: make 3 configurable
  inner.io.addr := io.addr(ADDR_WIDTH-1, 3) ## 0.U(3.W)
  inner.io.read <> io.read
  inner.io.axi <> io.axi
  inner.io.stall <> io.stall
  inner.io.pause <> io.pause

  val vecView = Wire(Vec(XLEN / 32, UInt(32.W)))
  vecView := inner.io.rdata.asTypeOf(vecView)
  io.data := vecView(pipeAddr(2).asUInt)

  inner.io.write := false.B
  inner.io.wdata := DontCare
  inner.io.be := DontCare

  io.vacant := inner.io.vacant || io.flush
  /*
  when(!io.pause) {
    printf(p"Before shift: ${Hexadecimal(inner.io.rdata)}\n")
  }
  */
}
