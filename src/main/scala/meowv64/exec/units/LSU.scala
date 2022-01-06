package meowv64.exec.units

import chisel3._
import chisel3.util._
import meowv64.instr.Decoder
import meowv64.cache._
import chisel3.experimental.ChiselEnum
import meowv64.exec._
import meowv64.core.CoreDef
import meowv64.core.ExType
import meowv64.core.ExReq
import meowv64.util.FlushableQueue
import meowv64.paging._
import meowv64.core.Satp
import meowv64.core.SatpMode
import meowv64.exec.UnitSel.Retirement
import scala.collection.mutable
import meowv64.core.PrivLevel
import meowv64.core.Status

/** DelayedMem = Delayed memory access, memory accesses that have side-effects
  * and thus needs to be preformed in-order.
  *
  * All possible access types are stated in the DelayedMemOp enum
  *   - no: No memory access
  *   - s: store
  *   - ul: uncached load
  *   - us: uncached store
  */
object DelayedMemOp extends ChiselEnum {
  val no, s, ul, us = Value
}

/** A (maybe-empty) sequential memory access
  *
  * op: operation addr: address, this is always aligned in log_2(coredef.XLEN)
  * offset: in-line offset len: Bit length sext: Sign extension?
  *
  * Data are stored in the wb field alongside the DelayedMem bundle in execution
  * results
  *
  * @param coredef:
  *   Core defination
  */
class DelayedMem(implicit val coredef: CoreDef) extends Bundle {
  self =>
  val op = DelayedMemOp()
  val wop = DCWriteOp()
  val addr = UInt(coredef.XLEN.W)
  val len = DCWriteLen()
  val sext = Bool()
  val data = UInt(coredef.XLEN.W)

  // Written data is shared with wb

  def noop() {
    self := DontCare
    op := DelayedMemOp.no
  }

  def isNoop() = op === DelayedMemOp.no

  def getLSB(raw: UInt): UInt = {
    val ret = Wire(UInt(coredef.XLEN.W))
    ret := DontCare
    when(sext) {
      val sret = Wire(SInt(coredef.XLEN.W))
      sret := DontCare
      switch(len) {
        is(DCWriteLen.B) {
          sret := raw(7, 0).asSInt()
        }
        is(DCWriteLen.H) {
          sret := raw(15, 0).asSInt()
        }
        is(DCWriteLen.W) {
          sret := raw(31, 0).asSInt()
        }
        is(DCWriteLen.D) {
          sret := raw(63, 0).asSInt()
        }
      }

      ret := sret.asUInt()
    }.otherwise {
      switch(len) {
        is(DCWriteLen.B) {
          ret := raw(7, 0)
        }
        is(DCWriteLen.H) {
          ret := raw(15, 0)
        }
        is(DCWriteLen.W) {
          ret := raw(31, 0)
        }
        is(DCWriteLen.D) {
          ret := raw(63, 0)
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

  def isUncached(addr: UInt) = addr < BigInt("80000000", 16).U

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
  val status = IO(Input(new Status))
  val ptw = IO(new TLBExt)
  val tlbrst = IO(Input(Bool()))
  val priv = IO(Input(PrivLevel()))

  val tlb = Module(new TLB)
  val requiresTranslate = satp.mode =/= SatpMode.bare && (
    priv <= PrivLevel.S
      || status.mprv && (status.mpp =/= PrivLevel.M.asUInt())
  )
  val tlbEffectivePriv =
    Mux(priv === PrivLevel.M, status.mpp.asTypeOf(PrivLevel.M), priv)
  val tlbMode = Wire(TLBLookupMode())
  when(tlbEffectivePriv === PrivLevel.U) {
    tlbMode := TLBLookupMode.U
  }.otherwise {
    tlbMode := Mux(status.sum, TLBLookupMode.both, TLBLookupMode.S)
  }

  val fenceLike = Wire(Bool())

  val tlbRequestModify = WireDefault(false.B)
  tlb.satp := satp
  tlb.ptw <> ptw
  tlb.query.query := requiresTranslate && !next.instr.vacant && !fenceLike
  tlb.query.isModify := tlbRequestModify
  tlb.query.mode := tlbMode
  tlb.flush := tlbrst

  val flushed = RegInit(false.B)

  val hasPending = IO(Output(Bool()))
  val release = IO(EnqIO(new DelayedMemResult))

  val pendings = Module(
    new FlushableQueue(new DelayedMem, coredef.INFLIGHT_INSTR_LIMIT)
  )
  hasPending := pendings.io.count > 0.U || pendings.io.enq.fire()

  val l2stall = Wire(Bool())
  val l1pass = Wire(Bool())

  pendings.io.flush := flush

  // Let's do this without helper

  // Utils
  def isInvalAddr(addr: UInt) = {
    val inval = WireDefault(
      addr(coredef.XLEN - 1, coredef.PADDR_WIDTH).asSInt() =/= addr(
        coredef.PADDR_WIDTH - 1
      ).asSInt()
    )

    when(requiresTranslate) {
      switch(satp.mode) {
        is(SatpMode.sv48) {
          inval := addr(coredef.XLEN - 1, coredef.VADDR_WIDTH)
            .asSInt() =/= addr(coredef.VADDR_WIDTH - 1).asSInt()
        }

        is(SatpMode.sv39) {
          inval := addr(coredef.XLEN - 1, coredef.VADDR_WIDTH - 9)
            .asSInt() =/= addr(coredef.VADDR_WIDTH - 10).asSInt()
        }
      }
    }
    inval
  }

  /** Is this access misaligned?
    *
    * According to ISA, the least two bits of funct3 repersents the size of the
    * load/store:
    *   - (L/S)B[U]: 00
    *   - (L/S)H[U]: 01
    *   - (L/S)W[U]: 10
    *   - (L/S)D: 11
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

  /** Stage 1 state
    */
  assert(coredef.PADDR_WIDTH > coredef.VADDR_WIDTH)
  val rawAddr =
    (next.rs1val.asSInt + next.instr.instr.imm).asUInt // We have imm = 0 for R-type instructions
  tlb.query.vpn := rawAddr(47, 12)
  val addr = WireDefault(rawAddr)
  when(requiresTranslate) {
    addr := tlb.query.ppn ## rawAddr(11, 0)
  }

  val aligned = addr(coredef.PADDR_WIDTH - 1, 3) ## 0.U(3.W)
  val offset = addr(2, 0)
  val uncached = isUncached(addr) && next.instr.instr.op =/= Decoder
    .Op("AMO")
    .ident // Uncached if phys bit 56 = high and is not AMO
  val read = (
    next.instr.instr.op === Decoder.Op("LOAD").ident
      || next.instr.instr.op === Decoder.Op("AMO").ident && next.instr.instr
        .funct7(6, 2) === Decoder.AMO_FUNC("LR")
  ) && !next.instr.vacant
  val write = (
    next.instr.instr.op === Decoder.Op("STORE").ident
      || next.instr.instr.op === Decoder.Op("AMO").ident && next.instr.instr
        .funct7(6, 2) =/= Decoder.AMO_FUNC("LR")
  ) && !next.instr.vacant

  fenceLike := (
    next.instr.instr.op === Decoder.Op("MEM-MISC").ident
      && !next.instr.vacant
  )

  val invalAddr = isInvalAddr(rawAddr)
  val misaligned = isMisaligned(offset, next.instr.instr.funct3)

  tlbRequestModify := write

  // Maybe we can let flushed request go through
  val canRead = WireDefault(
    read && !uncached && !invalAddr && !misaligned && !flush
  )
  when(requiresTranslate) {
    when(!tlb.query.ready) {
      canRead := false.B
    }

    when(tlb.query.fault) {
      canRead := false.B
    }
  }

  toMem.reader.req.bits.reserve := next.instr.instr.op =/= Decoder
    .Op("LOAD")
    .ident
  toMem.reader.req.bits.addr := aligned
  toMem.reader.req.valid := canRead

  when(next.instr.vacant) {
    l1pass := false.B
  }.elsewhen(fenceLike) {
    l1pass := !l2stall
  }.elsewhen(requiresTranslate && !tlb.query.ready) {
    l1pass := false.B
  }.elsewhen(toMem.reader.req.valid) {
    l1pass := toMem.reader.req.ready
  }.otherwise {
    l1pass := !l2stall // Not a DC access, has no waiting side effect, hence we can block indefinitely
  }

  when(l2stall) {
    assert(!toMem.reader.req.ready) // req.ready must be false
  }

  rs.pop := l1pass

  /** Stage 2 state
    */
  val pipeInstr = RegInit(PipeInstr.empty)
  val pipeRawAddr = Reg(UInt(coredef.XLEN.W))
  val pipeAddr = Reg(UInt(coredef.XLEN.W))
  val pipeFault = Reg(Bool())
  val pipeDCRead = Reg(Bool())
  val pipeInvalAddr = Reg(Bool())
  val pipeMisaligned = Reg(Bool())
  val pipeRead = Reg(Bool())
  val pipeWrite = Reg(Bool())

  l2stall := !toMem.reader.resp.valid && !pipeInstr.instr.vacant && pipeDCRead
  when(l1pass) {
    pipeFault := tlb.query.query && tlb.query.fault
    pipeInstr := next
    pipeAddr := addr
    pipeRawAddr := rawAddr
    pipeDCRead := toMem.reader.req.fire()
    pipeInvalAddr := invalAddr
    pipeMisaligned := misaligned
    pipeRead := read
    pipeWrite := write

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
  val pipeAMO = pipeInstr.instr.instr.op === Decoder.Op("AMO").ident
  val pipeUncached = isUncached(pipeAddr) && !pipeAMO
  val pipeFenceLike = (
    pipeInstr.instr.instr.op === Decoder.Op("MEM-MISC").ident
      && !pipeInstr.instr.vacant
  )

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

  val shifted =
    toMem.reader.resp.bits >> (pipeOffset << 3) // TODO: use lookup table?
  val signedResult = Wire(SInt(coredef.XLEN.W)).suggestName("signedResult")
  val result = Wire(UInt(coredef.XLEN.W)).suggestName("result")
  shifted.suggestName("shifted")
  result := signedResult.asUInt
  signedResult := DontCare

  switch(pipeInstr.instr.instr.funct3) {
    is(Decoder.MEM_WIDTH_FUNC("B")) { signedResult := shifted(7, 0).asSInt }
    is(Decoder.MEM_WIDTH_FUNC("H")) { signedResult := shifted(15, 0).asSInt }
    is(Decoder.MEM_WIDTH_FUNC("W")) { signedResult := shifted(31, 0).asSInt }
    is(Decoder.MEM_WIDTH_FUNC("D")) { result := shifted }
    is(Decoder.MEM_WIDTH_FUNC("BU")) { result := shifted(7, 0) }
    is(Decoder.MEM_WIDTH_FUNC("HU")) { result := shifted(15, 0) }
    is(Decoder.MEM_WIDTH_FUNC("WU")) { result := shifted(31, 0) }
  }

  // Retirement
  val mem = Wire(new DelayedMem)
  mem.noop() // By default

  when(pipeFenceLike) {
    retire.info.wb := DontCare
    when(pipeInstr.instr.instr.funct3 === Decoder.MEM_MISC_FUNC("FENCE.I")) {
      retire.info.branch.ifence(pipeInstr.instr.addr + 4.U)
    }.otherwise {
      retire.info.branch.nofire()
    }
  }.elsewhen(pipeInvalAddr) {
    retire.info.wb := pipeRawAddr
    retire.info.branch.ex(
      Mux(
        pipeRead,
        ExType.LOAD_ACCESS_FAULT,
        ExType.STORE_ACCESS_FAULT
      )
    )
  }.elsewhen(pipeFault) {
    retire.info.wb := pipeRawAddr
    retire.info.branch.ex(
      Mux(
        pipeRead,
        ExType.LOAD_PAGE_FAULT,
        ExType.STORE_PAGE_FAULT
      )
    )
  }.elsewhen(pipeMisaligned) {
    retire.info.wb := pipeRawAddr
    retire.info.branch.ex(
      Mux(
        pipeRead,
        ExType.LOAD_ADDR_MISALIGN,
        ExType.STORE_ADDR_MISALIGN
      )
    )
  }.elsewhen(pipeRead && !pipeUncached) {
    retire.info.branch.nofire()
    retire.info.wb := result
    when(pipeAMO) { // Must be LR
      mem.op := DelayedMemOp.s
      mem.wop := DCWriteOp.commitLR
      mem.addr := pipeAddr
      mem.data := result
    }
  }.elsewhen(pipeRead) {
    retire.info.branch.nofire()

    mem.op := DelayedMemOp.ul
    mem.addr := pipeAddr
    mem.sext := DontCare
    mem.len := DontCare
    switch(pipeInstr.instr.instr.funct3) {
      is(Decoder.MEM_WIDTH_FUNC("B")) {
        mem.sext := true.B
        mem.len := DCWriteLen.B
      }
      is(Decoder.MEM_WIDTH_FUNC("H")) {
        mem.sext := true.B
        mem.len := DCWriteLen.H
      }
      is(Decoder.MEM_WIDTH_FUNC("W")) {
        mem.sext := true.B
        mem.len := DCWriteLen.W
      }
      is(Decoder.MEM_WIDTH_FUNC("D")) {
        mem.sext := false.B
        mem.len := DCWriteLen.D
      }
      is(Decoder.MEM_WIDTH_FUNC("BU")) {
        mem.sext := false.B
        mem.len := DCWriteLen.B
      }
      is(Decoder.MEM_WIDTH_FUNC("HU")) {
        mem.sext := false.B
        mem.len := DCWriteLen.H
      }
      is(Decoder.MEM_WIDTH_FUNC("WU")) {
        mem.sext := false.B
        mem.len := DCWriteLen.W
      }
    }

    retire.info.wb := DontCare
    mem.data := DontCare
  }.elsewhen(pipeWrite) {
    retire.info.branch.nofire()

    mem.len := DontCare
    switch(pipeInstr.instr.instr.funct3) {
      is(Decoder.MEM_WIDTH_FUNC("B")) { mem.len := DCWriteLen.B }
      is(Decoder.MEM_WIDTH_FUNC("H")) { mem.len := DCWriteLen.H }
      is(Decoder.MEM_WIDTH_FUNC("W")) { mem.len := DCWriteLen.W }
      is(Decoder.MEM_WIDTH_FUNC("D")) { mem.len := DCWriteLen.D }
    }
    mem.addr := pipeAddr
    mem.sext := false.B

    when(pipeUncached) {
      mem.op := DelayedMemOp.us
    } otherwise {
      mem.op := DelayedMemOp.s
    }

    when(pipeAMO) {
      mem.wop := MuxLookup(
        pipeInstr.instr.instr.funct7(6, 2),
        DCWriteOp.idle,
        Seq(
          Decoder.AMO_FUNC("SC") -> DCWriteOp.cond,
          Decoder.AMO_FUNC("AMOSWAP") -> DCWriteOp.swap,
          Decoder.AMO_FUNC("AMOADD") -> DCWriteOp.add,
          Decoder.AMO_FUNC("AMOXOR") -> DCWriteOp.xor,
          Decoder.AMO_FUNC("AMOAND") -> DCWriteOp.and,
          Decoder.AMO_FUNC("AMOOR") -> DCWriteOp.or,
          Decoder.AMO_FUNC("AMOMIN") -> DCWriteOp.min,
          Decoder.AMO_FUNC("AMOMAX") -> DCWriteOp.max,
          Decoder.AMO_FUNC("AMOMINU") -> DCWriteOp.minu,
          Decoder.AMO_FUNC("AMOMAXU") -> DCWriteOp.maxu
        )
      )
    }.otherwise {
      mem.wop := DCWriteOp.write
    }

    retire.info.wb := DontCare
    mem.data := pipeInstr.rs2val
  }.otherwise {
    // Inval instr?
    retire.info.branch.ex(ExType.ILLEGAL_INSTR)
    retire.info.wb := DontCare
  }

  val push = mem.op =/= DelayedMemOp.no && !pipeInstr.instr.vacant && !l2stall
  retire.info.hasMem := push
  pendings.io.enq.bits := mem
  pendings.io.enq.valid := push
  assert(pendings.io.enq.ready)

  // Delayed memory ops
  val pendingHead = pendings.io.deq.bits
  toMem.writer.wdata := pendingHead.data
  toMem.uncached.wdata := pendingHead.data

  toMem.writer.addr := pendingHead.addr
  toMem.uncached.addr := pendingHead.addr

  // toMem.writer.be := pendingHead.be
  toMem.writer.len := pendingHead.len
  toMem.uncached.len := pendingHead.len

  toMem.writer.op := DCWriteOp.idle
  toMem.uncached.read := false.B
  toMem.uncached.write := false.B

  when(pendings.io.deq.valid && release.ready) {
    toMem.writer.op := Mux(
      pendingHead.op === DelayedMemOp.s,
      pendingHead.wop,
      DCWriteOp.idle
    )
    toMem.uncached.read := pendingHead.op === DelayedMemOp.ul
    toMem.uncached.write := pendingHead.op === DelayedMemOp.us
  }

  release.bits.data := DontCare
  switch(pendingHead.op) {
    is(DelayedMemOp.s) {
      release.bits.data := toMem.writer.rdata

      when(pendingHead.wop === DCWriteOp.commitLR) {
        release.bits.data := pendingHead.data
      }
    }

    is(DelayedMemOp.ul) {
      release.bits.data := pendingHead.getLSB(toMem.uncached.rdata)
    }
  }

  // FIXME: maybe we can ignore this, because for all such operations, we have erd =/= 0
  release.bits.hasWB := (
    pendingHead.op === DelayedMemOp.ul
      || pendingHead.op === DelayedMemOp.s && (
        pendingHead.wop =/= DCWriteOp.write
      )
  )

  val finished = (
    pendings.io.deq.valid
      && Mux(
        pendingHead.op === DelayedMemOp.s,
        !toMem.writer.stall,
        !toMem.uncached.stall
      )
  )
  release.valid := finished

  when(release.fire()) {
    pendings.io.deq.deq()
  } otherwise {
    pendings.io.deq.nodeq()
  }
}
