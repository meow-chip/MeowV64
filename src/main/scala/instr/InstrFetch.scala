package instr

import chisel3._
import _root_.cache._
import Decoder._
import _root_.core._
import _root_.data._
import chisel3.util.log2Ceil

class InstrExt(val ADDR_WIDTH: Int = 48) extends Bundle {
  val addr = UInt(ADDR_WIDTH.W)
  val instr = new Instr
  val vacant = Bool()

  override def toPrintable: Printable = {
    p"Address: 0x${Hexadecimal(addr)}\n" +
    p"Vacant: ${vacant}\n" +
    p"${instr}"
  }
}

class InstrFetch(ADDR_WIDTH: Int = 48, FETCH_NUM: Int = 1, DATA_WIDTH: Int = 64) extends Module {
  val io = IO(new Bundle {
    val pc = Input(UInt(ADDR_WIDTH.W))
    val skip = Input(UInt(log2Ceil(FETCH_NUM).W))

    val icache = Flipped(new ICachePort(ADDR_WIDTH, Const.INSTR_MIN_WIDTH * FETCH_NUM, DATA_WIDTH))
    val fetch = Input(Bool())
    val output = Output(Vec(FETCH_NUM, new InstrExt(ADDR_WIDTH)))

    val axi = new AXI(DATA_WIDTH)

    val ctrl = StageCtrl.stage()
  })

  io.ctrl.stall <> io.icache.stall
  io.ctrl.pause <> io.icache.pause
  io.ctrl.flush <> io.icache.flush
  io.axi <> io.icache.axi
  io.pc <> io.icache.addr
  io.fetch <> io.icache.read

  val pipePc = RegInit(0.U(ADDR_WIDTH.W))
  val pipeSkip = RegInit(0.U)
  when(!io.ctrl.stall && !io.ctrl.pause) {
    pipePc := io.pc
    pipeSkip := io.skip

    /*
    printf(p"IF:\n================\n")
    printf(p"Skipped: ${pipeSkip}\n\n")
    printf(p"Fetched:\n\n")
    for(instr <- io.output) {
      printf(p"${instr}\n\n")
    }
    */
  }

  val vecView = io.icache.data.asTypeOf(Vec(FETCH_NUM, UInt(Const.INSTR_MIN_WIDTH.W)))

  for(i <- (0 until FETCH_NUM)) {
    // Fuse two instructions together
    val higher = if(i == FETCH_NUM-1) {
      0.U(Const.INSTR_MIN_WIDTH.W)
    } else {
      vecView(i+1)
    }

    val full = higher ## vecView(i)
    val instr = full.asInstr
    io.output(i).instr

    // Skip this instr if last one was a full instr
    if(i != 0) {
      when(
        io.output(i-1).instr.base =/= InstrType.toInt(InstrType.C)
        && !io.output(i-1).vacant
      ) {
        io.output(i).vacant := true.B
      }
    }
  }

  for((wire, i) <- io.icache.data.asTypeOf(Vec(FETCH_NUM, UInt(32.W))).zipWithIndex) {
    io.output(i).instr := wire.asInstr
    io.output(i).addr := pipePc + (i * Const.INSTR_MIN_WIDTH / 8).U
    io.output(i).vacant := io.icache.vacant || i.U < pipeSkip 
  }
}
