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
import paging.TLB
import _root_.core.Satp
import _root_.core.SatpMode
import paging.TLBExt
import exec.UnitSel.Retirement
import scala.collection.mutable
import _root_.core.PrivLevel

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

class LSU(implicit val coredef: CoreDef) extends MultiIOModule with UnitSelIO {
  val flush = IO(Input(Bool()))
  val rs = IO(Flipped(new ResStationExgress))
  val retire = IO(Output(new Retirement))
  val extras = new mutable.HashMap[String, Data]()

  val hasNext = rs.valid // TODO: merge into rs.instr
  val next = WireDefault(rs.instr)
  when(!rs.valid) {
    next.instr.vacant := true.B
  }

  // FIXME: ValidIO resp
  val toMem = IO(new Bundle {
    val reader = new DCReader
    val writer = new DCWriter(coredef.L1D)
    val uncached = new L1UCPort(coredef.L1D)
  })

  val satp = IO(Input(new Satp))
  val ptw = IO(new TLBExt)
  val tlbrst = IO(Input(Bool()))
  val priv = IO(Input(PrivLevel()))

  val tlb = Module(new TLB)
  val requiresTranslate = satp.mode =/= SatpMode.bare && priv <= PrivLevel.S
  val tlbRequestModify = WireDefault(false.B)
  tlb.satp := satp
  tlb.ptw <> ptw
  tlb.query.query := requiresTranslate && !next.instr.vacant
  tlb.query.isModify := tlbRequestModify
  tlb.query.isUser := priv === PrivLevel.U
  tlb.flush := tlbrst

  val flushed = RegInit(false.B)

  val hasPending = IO(Output(Bool()))
  val release = IO(EnqIO(new DelayedMemResult))

  val pendings = Module(new FlushableQueue(new DelayedMem, coredef.INFLIGHT_INSTR_LIMIT))
  hasPending := pendings.io.count > 0.U || pendings.io.enq.fire()

  val l2stall = Wire(Bool())
  val l1pass = Wire(Bool())

  pendings.io.flush := flush

  // Let's do this without helper

  // Utils
  def isInvalAddr(addr: UInt) = {
    val inval = WireDefault(
      addr(coredef.XLEN-1, coredef.PADDR_WIDTH+1).orR // addr(coredef.PADDR_WIDTH) denotes uncached in bare/M mode
    )

    when(requiresTranslate) {
      switch(satp.mode) {
        is(SatpMode.sv48) {
          inval := addr(coredef.XLEN-1, coredef.VADDR_WIDTH).orR
        }

        is(SatpMode.sv39) {
          inval := addr(coredef.XLEN-1, coredef.VADDR_WIDTH - 9).orR
        }
      }
    }
    inval
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

  /**
   * Stage 1 state
   */ 
  assert(coredef.PADDR_WIDTH > coredef.VADDR_WIDTH)
  val rawAddr = (next.rs1val.asSInt + next.instr.instr.imm).asUInt
  tlb.query.vpn := rawAddr(47, 12)
  val addr = WireDefault(rawAddr)
  when(requiresTranslate) {
    addr := tlb.query.ppn ## rawAddr(11, 0)
  }

  val aligned = addr(coredef.PADDR_WIDTH - 1, 3) ## 0.U(3.W)
  val offset = addr(2, 0)
  val uncached = WireDefault(addr(coredef.PADDR_WIDTH)) // Use bit 56 to denote uncached in bare mode
  val read = next.instr.instr.op === Decoder.Op("LOAD").ident && !next.instr.vacant
  val write = next.instr.instr.op === Decoder.Op("STORE").ident && !next.instr.vacant

  when(requiresTranslate) {
    uncached := tlb.query.uncached
  }

  val invalAddr = isInvalAddr(addr)
  val misaligned = isMisaligned(offset, next.instr.instr.funct3)

  tlbRequestModify := write

  // Maybe we can let flushed request go through
  val canRead = WireDefault(read && !uncached && !invalAddr && !misaligned && !flush)
  when(requiresTranslate) {
    when(!tlb.query.ready) {
      canRead := false.B
    }

    when(tlb.query.fault) {
      canRead := false.B
    }
  }

  when(canRead) {
    toMem.reader.req.enq(aligned)
  }.otherwise {
    toMem.reader.req.noenq()
  }

  when(next.instr.vacant) {
    l1pass := false.B
  }.elsewhen(requiresTranslate && !tlb.query.ready) {
    l1pass := false.B
  }.elsewhen(toMem.reader.req.valid) {
    l1pass := toMem.reader.req.ready
  }.otherwise {
    l1pass := true.B
  }

  when(l2stall) {
    assert(!toMem.reader.req.ready) // req.ready must be false
  }

  rs.pop := l1pass

  /**
   * Stage 2 state
   */
  val pipeInstr = RegInit(PipeInstr.empty)
  val pipeAddr = Reg(UInt(coredef.XLEN.W))
  val pipeFault = Reg(Bool())
  val pipeDCRead = Reg(Bool())
  l2stall := !toMem.reader.resp.valid && !pipeInstr.instr.vacant && pipeDCRead
  when(l1pass) {
    pipeFault := tlb.query.fault
    pipeInstr := next
    pipeAddr := addr
    pipeDCRead := toMem.reader.req.fire()
    when(!flush) {
      assert(!l2stall)
    }
  }.elsewhen(!l2stall) {
    pipeInstr.instr.vacant := true.B
  }

  when(flush) {
    pipeInstr.instr.vacant := true.B
  }

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

  retire.instr := pipeInstr
  /*
   * We should be able to ignore keeping track of flushed here
   * Because if there was an flush, and an corresponding request is running inside DC,
   * then stage 1 requests will never be able to enter stage 2, because DC read ops are in-order
   * 
   * If we add MSHR, then we need to keep track of flushed requests
   */
  when(l2stall) {
    retire.instr.instr.vacant := true.B
  }

  val shifted = toMem.reader.resp.bits >> (pipeOffset << 3) // TODO: use lookup table?
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
    retire.info.wb := DontCare
    retire.info.branch.ifence(pipeInstr.instr.addr + 4.U)
  }.elsewhen(pipeInvalAddr) {
    retire.info.wb := pipeAddr
    retire.info.branch.ex(Mux(
      pipeRead,
      ExType.LOAD_ACCESS_FAULT,
      ExType.STORE_ACCESS_FAULT
    ))
  }.elsewhen(pipeFault) {
    retire.info.wb := pipeAddr
    retire.info.branch.ex(Mux(
      pipeRead,
      ExType.LOAD_PAGE_FAULT,
      ExType.STORE_PAGE_FAULT
    ))
  }.elsewhen(pipeMisaligned) {
    retire.info.wb := pipeAddr
    retire.info.branch.ex(Mux(
      pipeRead,
      ExType.LOAD_ADDR_MISALIGN,
      ExType.STORE_ADDR_MISALIGN
    ))
  }.elsewhen(pipeRead && !pipeUncached) {
    retire.info.branch.nofire()
    retire.info.wb := result
  }.elsewhen(pipeRead) {
    retire.info.branch.nofire()

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

    retire.info.wb := DontCare
    mem.data := DontCare
  }.elsewhen(pipeWrite) {
    retire.info.branch.nofire()

    mem.len := DontCare
    switch(pipeInstr.instr.instr.funct3) {
      is(Decoder.STORE_FUNC("SB")) { mem.len := DelayedMemLen.B }
      is(Decoder.STORE_FUNC("SH")) { mem.len := DelayedMemLen.H }
      is(Decoder.STORE_FUNC("SW")) { mem.len := DelayedMemLen.W }
      is(Decoder.STORE_FUNC("SD")) { mem.len := DelayedMemLen.D }
    }
    mem.offset := pipeOffset
    mem.addr := pipeAligned
    mem.sext := false.B

    when(pipeUncached) {
      mem.op := DelayedMemOp.us
    } otherwise {
      mem.op := DelayedMemOp.s
    }

    retire.info.wb := DontCare
    mem.data := pipeInstr.rs2val << (pipeOffset << 3)
  }.otherwise {
    // Inval instr?
    retire.info.branch.ex(ExType.ILLEGAL_INSTR)
    retire.info.wb := DontCare
  }

  val push = mem.op =/= DelayedMemOp.no && !pipeInstr.instr.vacant
  when(push) {
    assert(!l2stall)
  }
  retire.info.hasMem := push
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

  release.bits.data := pendingHead.getSlice(toMem.uncached.rdata)
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
