package exec.units

import chisel3._
import chisel3.util._
import instr.Decoder
import cache._
import chisel3.experimental.ChiselEnum
import exec._
import _root_.core.CoreDef
import _root_.core.ExType
import _root_.core.ExReq
import _root_.util.FlushableQueue

/**
 * DelayedMem = Delayed memory access, memory accesses that have side-effects and thus
 * needs to be preformed in-order.
 * 
 * All possible access types are stated in the DelayedMemOp enum
 * - no: No memory access
 * - s: store
 * - ul: uncached load
 * - us: uncached store
 */
object DelayedMemOp extends ChiselEnum {
  val no, s, ul, us = Value
}

/**
 * Bit length of the DelayedMem
 * 
 * Has the same defination as in the RISC-V unprivileged specification
 */
object DelayedMemLen extends ChiselEnum {
  val B, H, W, D = Value
}

/**
  * A (maybe-empty) sequential memory access
  * 
  * op: operation
  * addr: address, this is always aligned in log_2(coredef.XLEN)
  * offset: in-line offset
  * len: Bit length
  * sext: Sign extension?
  * 
  * Data are stored in the wb field alongside the DelayedMem bundle in execution results
  *
  * @param coredef: Core defination
  */
class DelayedMem(implicit val coredef: CoreDef) extends Bundle {
  self =>
  val op = DelayedMemOp()
  val addr = UInt(coredef.XLEN.W)
  val offset = UInt(log2Ceil(coredef.XLEN/8).W)
  val len = DelayedMemLen()
  val sext = Bool()
  val data = UInt(coredef.XLEN.W)

  // Written data is shared with wb

  def noop() {
    self := DontCare
    op := DelayedMemOp.no
  }

  def isNoop() = op === DelayedMemOp.no

  def be: UInt = {
    val raw = Wire(UInt((coredef.XLEN / 8).W))
    raw := DontCare
    switch(len) {
      is(DelayedMemLen.B) {
        raw := 0x1.U
      }
      is(DelayedMemLen.H) {
        raw := 0x3.U
      }
      is(DelayedMemLen.W) {
        raw := 0xf.U
      }
      is(DelayedMemLen.D) {
        raw := 0xff.U
      }
    }

    raw << offset
  }

  def getSlice(raw: UInt): UInt = {
    val shifted = raw >> (offset << 3)
    val ret = Wire(UInt(coredef.XLEN.W))
    ret := DontCare
    when(sext) {
      val sret = Wire(SInt(coredef.XLEN.W))
      sret := DontCare
      switch(len) {
        is(DelayedMemLen.B) {
          sret := shifted(7, 0).asSInt()
        }
        is(DelayedMemLen.H) {
          sret := shifted(15, 0).asSInt()
        }
        is(DelayedMemLen.W) {
          sret := shifted(31, 0).asSInt()
        }
        is(DelayedMemLen.D) {
          sret := shifted(63, 0).asSInt()
        }
      }

      ret := sret.asUInt()
    }.otherwise {
      switch(len) {
        is(DelayedMemLen.B) {
          ret := shifted(7, 0)
        }
        is(DelayedMemLen.H) {
          ret := shifted(15, 0)
        }
        is(DelayedMemLen.W) {
          ret := shifted(31, 0)
        }
        is(DelayedMemLen.D) {
          ret := shifted(63, 0)
        }
      }
    }

    ret
  }
}

class LSU(implicit val coredef: CoreDef) extends MultiIOModule with ExecUnitInt with exec.WithLSUPort {
  val DEPTH = 1

  val io = IO(new ExecUnitPort)

  val toMem = IO(new Bundle {
    val reader = new DCReader(coredef.L1D)
    val writer = new DCWriter(coredef.L1D)
    val uncached = new L1UCPort(coredef.L1D)
  })

  val flushed = RegInit(false.B)

  val hasPending = IO(Output(Bool()))
  val release = IO(EnqIO(new DelayedMemResult))

  val pendings = Module(new FlushableQueue(new DelayedMem, coredef.INFLIGHT_INSTR_LIMIT))
  hasPending := pendings.io.count > 0.U || pendings.io.enq.fire()

  toMem.reader := DontCare
  toMem.reader.read := false.B

  when(io.flush && toMem.reader.stall) {
    flushed := true.B
  }

  when(!toMem.reader.stall) {
    flushed := false.B
  }

  pendings.io.flush := io.flush

  // Let's do this without helper

  // Utils
  def isInvalAddr(addr: UInt) = if(coredef.XLEN == coredef.VADDR_WIDTH) { // TODO: physical mode
    false.B
  } else {
    addr(coredef.XLEN-1, coredef.VADDR_WIDTH).orR
  }

  /**
   * Is this access misaligned?
   * 
   * According to ISA, the least two bits of funct3 repersents the size of the load/store:
   * - (L/S)B[U]: 00
   * - (L/S)H[U]: 01
   * - (L/S)W[U]: 10
   * - (L/S)D: 11
   */
  def isMisaligned(offset: UInt, funct3: UInt) = Mux1H(
    UIntToOH(funct3(1, 0)),
    Seq(
      false.B,
      offset(0),
      offset(1, 0).orR,
      offset.orR
    )
  )

  val stall = toMem.reader.stall

  /**
   * Stage 1 state
   */ 
  assert(coredef.PADDR_WIDTH > coredef.VADDR_WIDTH)
  val addr = (io.next.rs1val.asSInt + io.next.instr.instr.imm).asUInt
  val aligned = addr(coredef.PADDR_WIDTH - 1, 3) ## 0.U(3.W)
  val offset = addr(2, 0)
  val uncached = addr(coredef.PADDR_WIDTH) // Use bit 56 to denote uncached
  val read = io.next.instr.instr.op === Decoder.Op("LOAD").ident && !io.next.instr.vacant
  val write = io.next.instr.instr.op === Decoder.Op("STORE").ident && !io.next.instr.vacant

  val invalAddr = isInvalAddr(addr)
  val misaligned = isMisaligned(offset, io.next.instr.instr.funct3)

  /**
   * Stage 2 state
   */
  val pipeInstr = RegInit(PipeInstr.empty)
  when(!stall) {
    pipeInstr := io.next
  }
  when(io.flush) {
    pipeInstr.instr.vacant := true.B
  }

  val pipeAddr = (pipeInstr.rs1val.asSInt + pipeInstr.instr.instr.imm).asUInt
  val pipeAligned = pipeAddr(coredef.PADDR_WIDTH - 1, 3) ## 0.U(3.W)
  val pipeOffset = pipeAddr(2, 0)
  val pipeUncached = pipeAddr(coredef.PADDR_WIDTH) // Use bit 56 to denote uncached
  val pipeRead = pipeInstr.instr.instr.op === Decoder.Op("LOAD").ident && !pipeInstr.instr.vacant
  val pipeWrite = pipeInstr.instr.instr.op === Decoder.Op("STORE").ident && !pipeInstr.instr.vacant
  val pipeFenceI = (
    pipeInstr.instr.instr.op === Decoder.Op("MEM-MISC").ident
    && pipeInstr.instr.instr.funct3 === Decoder.MEM_MISC_FUNC("FENCE.I")
    && !pipeInstr.instr.vacant
  )
  val pipeInvalAddr = isInvalAddr(pipeAddr)
  val pipeMisaligned = isMisaligned(pipeOffset, pipeInstr.instr.instr.funct3)

  io.retired := pipeInstr
  io.stall := stall
  when(io.flush) {
    io.retired.instr.vacant := true.B
  }

  // Reader
  toMem.reader.addr := aligned
  toMem.reader.read := read && !uncached && !invalAddr && !misaligned && !io.flush

  val shifted = toMem.reader.data >> (pipeOffset << 3) // TODO: use lookup table?
  val signedResult = Wire(SInt(coredef.XLEN.W)).suggestName("signedResult")
  val result = Wire(UInt(coredef.XLEN.W)).suggestName("result")
  shifted.suggestName("shifted")
  result := signedResult.asUInt
  signedResult := DontCare

  switch(pipeInstr.instr.instr.funct3) {
    is(Decoder.LOAD_FUNC("LB")) { signedResult := shifted(7, 0).asSInt }
    is(Decoder.LOAD_FUNC("LH")) { signedResult := shifted(15, 0).asSInt }
    is(Decoder.LOAD_FUNC("LW")) { signedResult := shifted(31, 0).asSInt }
    is(Decoder.LOAD_FUNC("LD")) { result := shifted }
    is(Decoder.LOAD_FUNC("LBU")) { result := shifted(7, 0) }
    is(Decoder.LOAD_FUNC("LHU")) { result := shifted(15, 0) }
    is(Decoder.LOAD_FUNC("LWU")) { result := shifted(31, 0) }
  }

  // Retirement
  val mem = Wire(new DelayedMem)
  mem.noop() // By default

  when(pipeFenceI) {
    io.retirement.wb := DontCare
    io.retirement.branch.ifence(pipeInstr.instr.addr + 4.U)
  }.elsewhen(pipeInvalAddr) {
    io.retirement.wb := pipeAddr
    io.retirement.branch.ex(Mux(
      pipeRead,
      ExType.LOAD_ACCESS_FAULT,
      ExType.STORE_ACCESS_FAULT
    ))
  }.elsewhen(pipeMisaligned) {
    io.retirement.wb := pipeAddr
    io.retirement.branch.ex(Mux(
      pipeRead,
      ExType.LOAD_ADDR_MISALIGN,
      ExType.STORE_ADDR_MISALIGN
    ))
  }.elsewhen(pipeRead && !pipeUncached) {
    io.retirement.branch.nofire()
    io.retirement.wb := result
  }.elsewhen(pipeRead) {
    io.retirement.branch.nofire()

    mem.op := DelayedMemOp.ul
    mem.addr := pipeAligned
    mem.offset := pipeOffset
    mem.sext := DontCare
    mem.len := DontCare
    switch(pipeInstr.instr.instr.funct3) {
      is(Decoder.LOAD_FUNC("LB")) {
        mem.sext := true.B
        mem.len := DelayedMemLen.B
      }
      is(Decoder.LOAD_FUNC("LH")) {
        mem.sext := true.B
        mem.len := DelayedMemLen.H
      }
      is(Decoder.LOAD_FUNC("LW")) {
        mem.sext := true.B
        mem.len := DelayedMemLen.W
      }
      is(Decoder.LOAD_FUNC("LD")) {
        mem.sext := false.B
        mem.len := DelayedMemLen.D
      }
      is(Decoder.LOAD_FUNC("LBU")) {
        mem.sext := false.B
        mem.len := DelayedMemLen.B
      }
      is(Decoder.LOAD_FUNC("LHU")) {
        mem.sext := false.B
        mem.len := DelayedMemLen.H
      }
      is(Decoder.LOAD_FUNC("LWU")) {
        mem.sext := false.B
        mem.len := DelayedMemLen.W
      }
    }

    io.retirement.wb := DontCare
    mem.data := DontCare
  }.elsewhen(pipeWrite) {
    io.retirement.branch.nofire()

    mem.len := DontCare
    switch(pipeInstr.instr.instr.funct3) {
      is(Decoder.STORE_FUNC("SB")) { mem.len := DelayedMemLen.B }
      is(Decoder.STORE_FUNC("SH")) { mem.len := DelayedMemLen.H }
      is(Decoder.STORE_FUNC("SW")) { mem.len := DelayedMemLen.W }
      is(Decoder.STORE_FUNC("SD")) { mem.len := DelayedMemLen.D }
    }
    mem.offset := pipeOffset
    // TODO: mask uncached bit
    mem.addr := pipeAligned
    mem.sext := false.B

    when(pipeUncached) {
      mem.op := DelayedMemOp.us
    } otherwise {
      mem.op := DelayedMemOp.s
    }

    io.retirement.wb := DontCare
    mem.data := pipeInstr.rs2val << (pipeOffset << 3)
  }.otherwise {
    // Inval instr?
    io.retirement.branch.ex(ExType.ILLEGAL_INSTR)
    io.retirement.wb := DontCare
  }

  val push = mem.op =/= DelayedMemOp.no && !io.retired.instr.vacant
  io.retirement.hasMem := push
  pendings.io.enq.bits := mem
  pendings.io.enq.valid := push
  assert(pendings.io.enq.ready)

  // Delayed memory ops
  val pendingHead = pendings.io.deq.bits
  toMem.writer.data := pendingHead.data
  toMem.uncached.wdata := pendingHead.data

  toMem.writer.addr := pendingHead.addr
  toMem.uncached.addr := pendingHead.addr
  
  toMem.writer.be := pendingHead.be
  toMem.uncached.wstrb := pendingHead.be

  toMem.writer.write := false.B
  toMem.uncached.read := false.B
  toMem.uncached.write := false.B

  when(pendings.io.deq.valid && release.ready) {
    toMem.writer.write := pendingHead.op === DelayedMemOp.s
    toMem.uncached.read := pendingHead.op === DelayedMemOp.ul
    toMem.uncached.write := pendingHead.op === DelayedMemOp.us
  }

  release.bits.data := toMem.uncached.rdata
  release.bits.isLoad := pendingHead.op === DelayedMemOp.ul

  val finished = (
    pendings.io.deq.valid
    && Mux(pendingHead.op === DelayedMemOp.s, !toMem.writer.stall, !toMem.uncached.stall)
  )
  release.valid := finished

  when(release.fire()) {
    pendings.io.deq.deq()
  } otherwise {
    pendings.io.deq.nodeq()
  }
}
