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
  val writer = IO(new CSRWriter())

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

  // compute new vstate
  val isVSETVL = instr.instr.op === Decoder.Op("OP-V").ident
  val newVState = WireInit(0.U.asTypeOf(new VState))
  val avl = WireInit(0.U(coredef.XLEN.W))
  val newVType = WireInit(0.U(coredef.XLEN.W))
  switch(instr.instr.raw(31, 30)) {
    is(0.U, 1.U) {
      // vsetvli
      avl := io.next.rs1val
      newVType := instr.instr.raw(30, 20)
    }
    is(2.U) {
      // vsetvl
      avl := io.next.rs1val
      newVType := io.next.rs2val
    }
    is(3.U) {
      // vsetivli
      avl := instr.instr.raw(19, 15)
      newVType := instr.instr.raw(29, 20)
    }
  }

  val setVLMAX = WireInit(instr.instr.rs1 === 0.U && instr.instr.rd =/= 0.U)
  val keepVL = WireInit(instr.instr.rs1 === 0.U && instr.instr.rd === 0.U)

  newVState.vtype.vma := newVType(7)
  newVState.vtype.vta := newVType(6)
  newVState.vtype.vsew := newVType(5, 3)
  newVState.vtype.vlmul := newVType(2, 0)

  // VLMAX = LMUL * VLEN / SEW
  // SEW = (1 << VSEW) * 8
  // LMUL = 1 << VLMUL
  // TODO: fractional LMUL
  val vlmax =
    ((coredef.VLEN.U << newVState.vtype.vlmul) >> newVState.vtype.vsew) >> 3
  when(setVLMAX) {
    newVState.vl := vlmax
  }.elsewhen(keepVL) {
    newVState.vl := writer.currentVState.vl
  }.otherwise {
    // simplify
    // 1. vl = AVL if AVL <= VLMAX
    // 2. vl = VLMAX if AVL >= VLMAX
    when(avl <= vlmax) {
      newVState.vl := avl
    }.otherwise {
      newVState.vl := vlmax
    }
  }

  // Check privileges
  val ro = addr(11, 10) === 3.U
  val minPriv = addr(9, 8).asTypeOf(PrivLevel())

  val wdata = RegInit(0.U(coredef.XLEN.W))
  val wdiff = Wire(UInt(coredef.XLEN.W))
  wdiff := DontCare

  writer.write := false.B
  writer.wdata := wdata
  writer.updateVState.valid := false.B
  writer.updateVState.bits := 0.U.asTypeOf(new VState)

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

  when(isVSETVL) {
    rdata := newVState.vl
  }

  when(!fault && io.next.instr.valid) {
    nstate := CSRState.pipe
  }

  /* Stage 2 */
  val pipeAddr = RegNext(addr)
  val pipeRdata = RegNext(rdata)
  // when no fault occurred
  // send branch to npc to flush pipeline
  val pipeWritten = RegNext(!fault)
  when(state === CSRState.pipe) {
    when(RegNext(isVSETVL)) {
      writer.updateVState.valid := true.B
      writer.updateVState.bits := RegNext(newVState)

      nstate := CSRState.read
    }.otherwise {
      writer.addr := pipeAddr
      writer.write := true.B
      nstate := CSRState.read
    }
  }

  val info = WireDefault(RetireInfo.vacant)

  when(fault) {
    info.branch.ex(ExType.ILLEGAL_INSTR)
  }.elsewhen(pipeWritten) {
    // csr is updated
    // flush pipeline to avoid stale value
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
