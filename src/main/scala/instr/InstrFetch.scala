package instr

import chisel3._
import _root_.cache._
import Decoder._
import _root_.core._
import _root_.data._

class InstrExt(val ADDR_WIDTH: Int = 48) extends Bundle {
  val addr = UInt(ADDR_WIDTH.W)
  val instr = new Instr

  override def toPrintable: Printable = {
    p"Address: 0x${Hexadecimal(addr)}\n" +
    p"${instr}"
  }
}

class InstrFetch(ADDR_WIDTH: Int = 48, FETCH_NUM: Int = 1) extends Module {
  val io = IO(new Bundle {
    val pc = Input(UInt(ADDR_WIDTH.W))
    val icache = Flipped(new ICachePort(ADDR_WIDTH, 32 * FETCH_NUM))
    val fetch = Input(Bool())
    val output = Output(Vec(FETCH_NUM, new InstrExt(ADDR_WIDTH)))

    val axi = new AXI(8)

    val ctrl = StageCtrl.stage()
  })

  /*
  printf(p"IF:\n================\n")
  printf(p"Fetched: ${io.output(0)}\n\n")
  */

  io.ctrl.stall <> io.icache.stall
  io.ctrl.pause <> io.icache.pause
  io.ctrl.flush <> io.icache.flush
  io.axi <> io.icache.axi
  io.pc <> io.icache.addr
  io.fetch <> io.icache.read

  val pipePc = RegInit(0.U(ADDR_WIDTH.W))
  pipePc := io.pc

  for((wire, i) <- io.icache.data.asTypeOf(Vec(FETCH_NUM, UInt(32.W))).zipWithIndex) {
    io.output(i).instr := wire.asInstr
    io.output(i).addr := pipePc + i.U
  }
}
