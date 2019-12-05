package exec

import core.CoreDef
import chisel3._
import chisel3.util._
import chisel3.MultiIOModule
import instr.InstrExt
import org.scalatest.tools.RerunningState
import exec._
import chisel3.util.log2Ceil
import chisel3.experimental.ChiselEnum
import _root_.core.ExReq
import _root_.core.ExType
import _root_.core.CSRWriter
import chisel3.util.MuxLookup

class BranchResult(implicit val coredef: CoreDef) extends Bundle {
  val branch = Bool()
  val target = UInt(coredef.ADDR_WIDTH.W)

  val ex = ExReq()
  val extype = ExType()

  def nofire() = {
    branch := false.B
    target := DontCare

    ex := ExReq.none
    extype := DontCare
  }

  def fire(addr: UInt) = {
    branch := true.B
    target := addr

    ex := ExReq.none
    extype := DontCare
  }

  def ex(et: ExType.Type) {
    branch := false.B
    target := DontCare

    ex := ExReq.ex
    extype := et
  }

  def eret() {
    branch := false.B
    target := DontCare

    ex := ExReq.ret
    extype := DontCare
  }

  def branched(): Bool = branch || ex =/= ExReq.none
}

object BranchResult {
  def empty(implicit coredef: CoreDef): BranchResult = {
    val ret = Wire(new BranchResult)
    ret.nofire()
    ret
  }
}

object MemSeqAccOp extends ChiselEnum {
  val no, s, ul, us = Value
}

object MemSeqAccLen extends ChiselEnum {
  val B, H, W, D = Value
}

class MemSeqAcc(implicit val coredef: CoreDef) extends Bundle {
  self =>
  val op = MemSeqAccOp()
  val addr = UInt(coredef.XLEN.W)
  val offset = UInt(log2Ceil(coredef.XLEN/8).W)
  val len = MemSeqAccLen()
  val sext = Bool()

  // Written data is shared with wb

  def noop() {
    self := DontCare
    op := MemSeqAccOp.no
  }

  def isNoop() = op === MemSeqAccOp.no

  def computeBe(): UInt = {
    val raw = Wire(UInt((coredef.XLEN / 8).W))
    raw := DontCare
    switch(len) {
      is(MemSeqAccLen.B) {
        raw := 0x1.U
      }
      is(MemSeqAccLen.H) {
        raw := 0x3.U
      }
      is(MemSeqAccLen.W) {
        raw := 0xf.U
      }
      is(MemSeqAccLen.D) {
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
        is(MemSeqAccLen.B) {
          sret := shifted(7, 0).asSInt()
        }
        is(MemSeqAccLen.H) {
          sret := shifted(15, 0).asSInt()
        }
        is(MemSeqAccLen.W) {
          sret := shifted(31, 0).asSInt()
        }
        is(MemSeqAccLen.D) {
          sret := shifted(63, 0).asSInt()
        }
      }

      ret := sret.asUInt()
    }.otherwise {
      switch(len) {
        is(MemSeqAccLen.B) {
          ret := shifted(7, 0)
        }
        is(MemSeqAccLen.H) {
          ret := shifted(15, 0)
        }
        is(MemSeqAccLen.W) {
          ret := shifted(31, 0)
        }
        is(MemSeqAccLen.D) {
          ret := shifted(63, 0)
        }
      }
    }

    ret
  }
}

class RetireInfo(implicit val coredef: CoreDef) extends Bundle {
  val wb = UInt(coredef.XLEN.W)
  val branch = new BranchResult
  val mem = new MemSeqAcc
}

object RetireInfo {
  def vacant(implicit coredef: CoreDef): RetireInfo = {
    val info = Wire(new RetireInfo)

    info.branch.nofire()
    info.mem.noop()
    info.wb := DontCare

    info
  }
}

class RenamedInstr(implicit val coredef: CoreDef) extends Bundle {
  val name = UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)
  val data = UInt(coredef.XLEN.W)
  val ready = Bool()
}

class PipeInstr(implicit val coredef: CoreDef) extends Bundle {
  val instr = new InstrExt(coredef.ADDR_WIDTH)

  val rs1val = UInt(coredef.XLEN.W)
  val rs2val = UInt(coredef.XLEN.W)

  val rdname = UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)
  val tag = UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)
}

class ReservedInstr(override implicit val coredef: CoreDef) extends PipeInstr {
  val rs1name = UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)
  val rs2name = UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)
  val rs1ready = Bool()
  val rs2ready = Bool()

  def ready = rs1ready && rs2ready

  def validate() {
    // Asserts that name === 0 -> ready
    assert(rs1name =/= 0.U || rs1ready)
    assert(rs2name =/= 0.U || rs2ready)
  }
}

object PipeInstr {
  def empty(implicit coredef: CoreDef): PipeInstr = {
    val ret = Wire(new PipeInstr)
    ret.instr := InstrExt.empty(coredef.ADDR_WIDTH)
    ret.rs1val := DontCare
    ret.rs2val := DontCare
    ret.rdname := DontCare
    ret.tag := DontCare

    ret
  }
}

object ReservedInstr {
  def empty(implicit coredef: CoreDef): ReservedInstr = {
    val ret = Wire(new ReservedInstr)
    ret := DontCare
    ret.instr := InstrExt.empty(coredef.ADDR_WIDTH)

    ret
  }
}

class ExecUnitPort(implicit val coredef: CoreDef) extends Bundle {
  val next = Input(new PipeInstr)

  val stall = Output(Bool())
  val pause = Input(Bool())
  val flush = Input(Bool())

  val retirement = Output(new RetireInfo)
  val retired = Output(new PipeInstr)
}

trait ExecUnitInt {
  val DEPTH: Int
  val io: ExecUnitPort
}

abstract class ExecUnit[T <: Data](
  val DEPTH: Int,
  val ExtData: T
)(
  implicit val coredef: CoreDef
) extends MultiIOModule with ExecUnitInt {
  val io = IO(new ExecUnitPort)

  var current = if(DEPTH != 0) {
    val storeInit = Wire(Vec(DEPTH, new Bundle {
      val pipe = new PipeInstr
      val ext = ExtData.cloneType
    }))

    for(i <- (0 until DEPTH)) {
      storeInit(i) := DontCare
      storeInit(i).pipe.instr.instr.imm := 0.S // Treadle bug?
      storeInit(i).pipe.instr.vacant := true.B
      storeInit(i).ext := DontCare
    }

    RegInit(storeInit)
  } else {
    null
  }

  def init(): Unit = {
    if(DEPTH != 0) {
      val (fExt, fStall) = connectStage(0, io.next, None)
      var stall = fStall

      when(!io.stall && !io.pause) {
        current(0).pipe := io.next
        current(0).ext := fExt
      }

      for(i <- (1 until DEPTH)) {
        val (nExt, sStall) = connectStage(i, current(i-1).pipe, Some(current(i-1).ext))
        when(!io.stall && !io.pause) {
          current(i).pipe := current(i-1).pipe
          current(i).ext := nExt
        }

        when(sStall) {
          current(i-1).ext := nExt
        }

        stall = stall || sStall
      }

      val (nExt, lStall) = connectStage(DEPTH, current(DEPTH-1).pipe, Some(current(DEPTH-1).ext))

      when(lStall) {
        current(DEPTH-1).ext := nExt
      }

      when(io.flush) { // Override current
        for(c <- current) {
          c.pipe := PipeInstr.empty
          c.ext := DontCare
        }
      }

      io.retired := current(DEPTH-1).pipe
      when(io.retired.instr.vacant) {
        io.retirement := RetireInfo.vacant
      }.otherwise {
        io.retirement := finalize(current(DEPTH-1).pipe, nExt)
      }
      io.stall := stall || lStall
    } else {
      val (nExt, sStall) = connectStage(0, io.next, None)
      // Use chisel's unconnected wire check to enforce that no ext is exported from this exec unit
      io.retired := io.next
      when(io.retired.instr.vacant) {
        io.retirement := RetireInfo.vacant
      }.otherwise {
        io.retirement := finalize(io.next, nExt)
      }
      io.stall := sStall
    }
  }

  def map(stage: Int, pipe: PipeInstr, ext: Option[T]): (T, Bool)

  def finalize(pipe: PipeInstr, ext: T): RetireInfo 

  def connectStage(stage: Int, pipe: PipeInstr, ext: Option[T]): (T, Bool) = {
    val nExt = Wire(ExtData.cloneType)
    val sStall = Wire(Bool())

    when(pipe.instr.vacant) {
      nExt := DontCare
      sStall := false.B
    }.otherwise {
      val (ce, stall) = map(stage, pipe, ext)
      nExt := ce
      sStall := stall
    }

    (nExt, sStall)
  }
}

class CDBEntry(implicit val coredef: CoreDef) extends Bundle {
  val valid = Bool()
  val name = UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)
  val data = UInt(coredef.XLEN.W)
}

class CDB(implicit val coredef: CoreDef) extends Bundle {
  val entries = Vec(coredef.UNIT_COUNT+1, new CDBEntry)
}
