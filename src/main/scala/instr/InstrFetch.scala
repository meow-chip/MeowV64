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

class InstrFetch(coredef: CoreDef) extends Module {
  val io = IO(new Bundle {
    val pc = Input(UInt(coredef.ADDR_WIDTH.W))
    val skip = Input(UInt(log2Ceil(coredef.FETCH_NUM).W))

    val icache = Flipped(new ICPort(coredef.L1I))
    val fetch = Input(Bool())
    val output = Output(Vec(coredef.FETCH_NUM, new InstrExt(coredef.ADDR_WIDTH)))

    val ctrl = StageCtrl.stage()
    val rst = Input(Bool())
  })

  io.icache.rst <> io.rst
  io.ctrl.stall <> io.icache.stall
  io.ctrl.pause <> DontCare // FIXME: impl issue fifo
  io.ctrl.flush <> io.icache.flush
  io.pc <> io.icache.addr
  io.fetch <> io.icache.read

  val tailFailed = RegInit(false.B)
  val tail = RegInit(0.U(Const.INSTR_MIN_WIDTH.W))

  val pipePc = RegInit(0.U(coredef.ADDR_WIDTH.W))
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

  val vecView = io.icache.data.asTypeOf(Vec(coredef.FETCH_NUM, UInt(Const.INSTR_MIN_WIDTH.W)))

  for(i <- (0 until coredef.FETCH_NUM)) {
    io.output(i).addr := pipePc + (i*Const.INSTR_MIN_WIDTH/8).U
    io.output(i).vacant := io.icache.vacant || i.U < pipeSkip

    if(i == coredef.FETCH_NUM-1) {
      val (parsed, success) = vecView(i).tryAsInstr16
      io.output(i).instr := parsed

      when(!io.icache.vacant && !success) {
        io.output(i).vacant := true.B

        when(!io.ctrl.pause && !io.ctrl.stall) {
          tailFailed := true.B
          tail := vecView(i)
        }
      }.otherwise {
        when(!io.ctrl.pause && !io.ctrl.stall) {
          tailFailed := false.B
        }
      }
    } else if(i == 0) {
      when(tailFailed) {
        val full = vecView(0) ## tail
        io.output(0).instr := full.asInstr
        io.output(0).addr := pipePc - (Const.INSTR_MIN_WIDTH/8).U
      }.otherwise {
        val full = vecView(i+1) ## vecView(i)
        io.output(i).instr := full.asInstr
      }
    } else {
      // Fuse two instructions together
      val full = vecView(i+1) ## vecView(i)
      io.output(i).instr := full.asInstr
    }

    // Skip this instr if last one was a full instr
    if(i != 0) {
      var cond = io.output(i-1).instr.base =/= InstrType.toInt(InstrType.C) && !io.output(i-1).vacant
      if(i == 1) cond = cond && !tailFailed
      when(cond) {
        io.output(i).vacant := true.B

        if(i == coredef.FETCH_NUM-1) {
          when(!io.ctrl.pause && !io.ctrl.stall) {
            tailFailed := false.B
          }
        }
      }
    }
  }
}
