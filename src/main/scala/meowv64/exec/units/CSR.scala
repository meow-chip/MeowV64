package meowv64.exec.units

import chisel3._
import chisel3.experimental._
import chisel3.util._
import meowv64.core._
import meowv64.exec._
import meowv64.instr.Decoder

class CSRExt(implicit val coredef: CoreDef) extends Bundle {
  val rdata = UInt(coredef.XLEN.W)
  val fault = Bool()
  var written = Bool()
}

class CSR(implicit val coredef: CoreDef)
    extends Module
    with ExecUnitInt
    with WithCSRWriter
    with WithPrivPort
    with WithStatus {
  val DEPTH: Int = 0

  object CSRState extends ChiselEnum {
    val read, pipe = Value
  }

  val io = IO(new ExecUnitPort)
  val priv = IO(Input(PrivLevel()))
  val status = IO(Input(new Status))
  val writer = IO(new CSRWriter(coredef.XLEN))

  val state = RegInit(CSRState.read)
  val nstate = WireDefault(state)
  state := nstate

  // CSR operations cannot happen when the pipeline is not empty
  assert(
    !(io.flush && io.next.instr.valid)
  )
  val instr = io.next.instr
  val addr = instr.instr.funct7 ## instr.instr.rs2
  writer.addr := addr

  // Check privileges
  val ro = addr(11, 10) === 3.U
  val minPriv = addr(9, 8).asTypeOf(PrivLevel())

  val wdata = RegInit(0.U(coredef.XLEN.W))
  val wdiff = Wire(UInt(coredef.XLEN.W))
  wdiff := DontCare

  writer.write := false.B
  writer.wdata := wdata

  switch(instr.instr.funct3) {
    is(
      Decoder.SYSTEM_FUNC("CSRRW"),
      Decoder.SYSTEM_FUNC("CSRRS"),
      Decoder.SYSTEM_FUNC("CSRRC")
    ) {
      wdiff := io.next.rs1val
    }

    is(
      Decoder.SYSTEM_FUNC("CSRRWI"),
      Decoder.SYSTEM_FUNC("CSRRSI"),
      Decoder.SYSTEM_FUNC("CSRRCI")
    ) {
      wdiff := instr.instr.rs1
    }
  }

  val written = WireDefault(wdiff =/= 0.U)
  switch(instr.instr.funct3) {
    is(Decoder.SYSTEM_FUNC("CSRRW"), Decoder.SYSTEM_FUNC("CSRRWI")) {
      wdata := wdiff
      written := true.B
    }

    is(Decoder.SYSTEM_FUNC("CSRRS"), Decoder.SYSTEM_FUNC("CSRRSI")) {
      wdata := writer.rdata | wdiff
    }

    is(Decoder.SYSTEM_FUNC("CSRRC"), Decoder.SYSTEM_FUNC("CSRRCI")) {
      wdata := writer.rdata & (~wdiff)
    }
  }

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

  when(!fault && io.next.instr.valid) {
    nstate := CSRState.pipe
  }

  /* Stage 2 */
  val pipeAddr = RegNext(addr)
  val pipeRdata = RegNext(rdata)
  val pipeWritten = RegNext(fault)
  when(state === CSRState.pipe) {
    writer.addr := pipeAddr
    writer.write := true.B
    nstate := CSRState.read
  }

  val info = WireDefault(RetireInfo.vacant)

  when(fault) {
    info.branch.ex(ExType.ILLEGAL_INSTR)
  }.elsewhen(pipeWritten) {
    info.branch.fire(instr.addr + 4.U)
    info.wb := pipeRdata
  } otherwise {
    info.branch.nofire
    info.wb := pipeRdata
  }

  io.retired := io.next
  io.retirement := info

  io.stall := nstate === CSRState.pipe
  when(io.flush) {
    nstate := CSRState.read
  }
}
