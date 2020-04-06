package exec.units

import chisel3._
import chisel3.util._
import instr.Decoder
import _root_.core._
import exec._

class CSRExt(implicit val coredef: CoreDef) extends Bundle {
  val rdata = UInt(coredef.XLEN.W)
  val fault = Bool()
  var written = Bool()
}

class CSR(implicit val coredef: CoreDef)
  extends MultiIOModule with ExecUnitInt with WithCSRWriter with WithPrivPort with WithStatus
{
  val DEPTH: Int = 0

  val io = IO(new ExecUnitPort)
  val priv = IO(Input(PrivLevel()))
  val status = IO(Input(new Status))
  val writer = IO(new CSRWriter(coredef.XLEN))

  assert(!io.flush || io.next.instr.vacant) // CSR operations cannot happen when the pipeline is not empty
  val instr = io.next.instr
  val addr = instr.instr.funct7 ## instr.instr.rs2
  writer.addr := addr

  // Check privileges
  val ro = addr(11, 10) === 3.U
  val minPriv = addr(9, 8).asTypeOf(PrivLevel())

  val op = WireDefault(CSROp.rs)
  val wdata = WireDefault(0.U)

  writer.op := CSROp.rs
  writer.wdata := 0.U
  
  switch(instr.instr.funct3) {
    is(Decoder.SYSTEM_FUNC("CSRRW")) {
      op := CSROp.rw
      wdata := io.next.rs1val
    }

    is(Decoder.SYSTEM_FUNC("CSRRWI")) {
      op := CSROp.rw
      wdata := instr.instr.rs1
    }

    is(Decoder.SYSTEM_FUNC("CSRRS")) {
      op := CSROp.rs
      wdata := io.next.rs1val
    }

    is(Decoder.SYSTEM_FUNC("CSRRSI")) {
      op := CSROp.rs
      wdata := instr.instr.rs1
    }

    is(Decoder.SYSTEM_FUNC("CSRRC")) {
      op := CSROp.rc
      wdata := io.next.rs1val
    }

    is(Decoder.SYSTEM_FUNC("CSRRCI")) {
      op := CSROp.rc
      wdata := instr.instr.rs1
    }
  }

  val written = op === CSROp.rw || wdata =/= 0.U
  val fault = WireDefault(false.B)
  val rdata = Wire(UInt(coredef.XLEN.W))
  when((ro && written) || priv < minPriv) {
    fault := true.B
    rdata := DontCare
  }.elsewhen((addr === 0x180.U) && status.tvm) { // SATP trap
    fault := true.B
    rdata := DontCare
  }.otherwise {
    fault := false.B
    rdata := writer.rdata
  }

  when(!fault && !io.next.instr.vacant) {
    writer.wdata := wdata
    writer.op := op
  }

  val info = WireDefault(RetireInfo.vacant)

  when(fault) {
    info.branch.ex(ExType.ILLEGAL_INSTR)
  }.elsewhen(written) {
    info.branch.fire(instr.addr + 4.U)
    info.wb := rdata
  } otherwise {
    info.branch.nofire()
    info.wb := rdata
  }

  io.retired := io.next
  io.retirement := info
  io.flush := DontCare
  io.stall := false.B
}
