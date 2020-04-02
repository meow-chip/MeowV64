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

class CSR(override implicit val coredef: CoreDef)
  extends ExecUnit(0, new CSRExt) with WithCSRWriter with WithPrivPort with WithStatus
{
  val priv = IO(Input(PrivLevel()))
  val status = IO(Input(new Status))
  val writer = IO(new CSRWriter(coredef.XLEN))
  writer.op := CSROp.rs
  writer.addr := 0.U
  writer.wdata := 0.U

  override def map(stage: Int, pipe: PipeInstr, ext: Option[CSRExt]): (CSRExt, Bool) = {
    // Asserts pipe.op === SYSTEM

    val ext = Wire(new CSRExt)
    val addr = pipe.instr.instr.funct7 ## pipe.instr.instr.rs2
    writer.addr := addr

    // Check privileges
    val ro = addr(11, 10) === 3.U
    val minPriv = addr(9, 8).asTypeOf(PrivLevel())

    val op = Wire(CSROp())
    val wdata = Wire(UInt(coredef.XLEN.W))
    op := CSROp.rs
    wdata := 0.U

    switch(pipe.instr.instr.funct3) {
      is(Decoder.SYSTEM_FUNC("CSRRW")) {
        op := CSROp.rw
        wdata := pipe.rs1val
      }

      is(Decoder.SYSTEM_FUNC("CSRRWI")) {
        op := CSROp.rw
        wdata := pipe.instr.instr.rs1
      }

      is(Decoder.SYSTEM_FUNC("CSRRS")) {
        op := CSROp.rs
        wdata := pipe.rs1val
      }

      is(Decoder.SYSTEM_FUNC("CSRRSI")) {
        op := CSROp.rs
        wdata := pipe.instr.instr.rs1
      }

      is(Decoder.SYSTEM_FUNC("CSRRC")) {
        op := CSROp.rc
        wdata := pipe.rs1val
      }

      is(Decoder.SYSTEM_FUNC("CSRRCI")) {
        op := CSROp.rc
        wdata := pipe.instr.instr.rs1
      }
    }

    ext.written := op === CSROp.rw || wdata =/= 0.U
    when((ro && ext.written) || priv < minPriv) {
      ext.fault := true.B
      ext.rdata := DontCare
    }.elsewhen((addr === 0x180.U) && status.tvm) { // SATP trap
      ext.fault := true.B
      ext.rdata := DontCare
    }.otherwise {
      ext.fault := false.B
      writer.wdata := wdata
      writer.op := op
      ext.rdata := writer.rdata
    }

    (ext, false.B)
  }

  override def finalize(pipe: PipeInstr, ext: CSRExt): RetireInfo = {
    val info = Wire(new RetireInfo)

    when(ext.fault) {
      info.branch.ex(ExType.ILLEGAL_INSTR)
      info.mem.noop()
      info.wb := DontCare
    }.elsewhen(ext.written) {
      info.branch.fire(pipe.instr.addr + 4.U)
      info.mem.noop()
      info.wb := ext.rdata
    } otherwise {
      info.branch.nofire()
      info.mem.noop()
      info.wb := ext.rdata
    }

      info
  }

  init()
}
