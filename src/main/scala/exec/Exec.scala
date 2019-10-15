package exec
import chisel3._
import reg._
import instr._
import chisel3.util._

class BranchResult(ADDR_WIDTH: Int = 48) extends Bundle {
  val branch = Bool()
  val target = UInt(ADDR_WIDTH.W)
}

class Exec extends Module {
  val io = IO(new Bundle {
    val regReader = new RegReader
    val regWriter = new RegWriter
    val instr = Input(new InstrExt)

    val pause = Input(Bool())
    val stall = Output(Bool())

    val branch = Output(new BranchResult)
  })

  io.stall := false.B
  io.branch := 0.U
  io.regWriter.addr := 0.U

  val current = Reg(new InstrExt)

  when(!io.pause) {
    current := io.instr
  }

  switch(current.instr.op) {
    is(Decoder.Op("LUI").ident) {
      io.regWriter.addr := current.instr.rd
      io.regWriter.data.asSInt := current.instr.imm
    }

    is(Decoder.Op("AUIPC").ident) {
      io.regWriter.addr := current.instr.rd
      io.regWriter.data.asSInt := current.instr.imm + current.addr.asSInt
    }
  }
}