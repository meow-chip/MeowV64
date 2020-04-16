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
import instr.Decoder.InstrType
import cache.DCReader
import _root_.core.PrivLevel
import _root_.core.Status
import cache.DCWriter
import cache.L1UCPort
import instr.Decoder
import instr.BHTPrediction
import instr.BPUResult
import paging.TLBExt
import _root_.core.Satp

/**
  * Branch result
  * 
  * Branch are used to state an interrupt in pipelined execution, which comes from
  * the execution of an instruction. Following conditions are considered as a branch:
  * - Jumps and branches that don't match branch prediction results
  * - Instructions caused an exceptions
  * - Instructions have side-effects on previous stages, so that we need to flush the pipeline.
  *   This includes:
  *   - CSR writes (may alter instr fetch)
  *   - FENCE.I
  *
  * @param coredef: Core defination
  */
class BranchResult(implicit val coredef: CoreDef) extends Bundle {
  val branch = Bool()
  val irst = Bool()
  val tlbrst = Bool()
  val target = UInt(coredef.XLEN.W)

  val ex = ExReq()
  val extype = ExType()

  def nofire() = {
    branch := false.B
    target := DontCare
    irst := false.B
    tlbrst := false.B

    ex := ExReq.none
    extype := DontCare
  }

  def fire(addr: UInt) = {
    branch := true.B
    target := addr
    irst := false.B
    tlbrst := false.B

    ex := ExReq.none
    extype := DontCare
  }

  def ifence(addr: UInt) = {
    branch := true.B
    target := addr
    irst := true.B
    tlbrst := false.B

    ex := ExReq.none
    extype := DontCare
  }

  def sfence(addr: UInt) = {
    branch := true.B
    target := addr
    irst := false.B
    tlbrst := true.B

    ex := ExReq.none
    extype := DontCare
  }

  def ex(et: ExType.Type) {
    branch := false.B
    target := DontCare
    irst := false.B
    tlbrst := false.B

    ex := ExReq.ex
    extype := et
  }

  def mret() {
    branch := false.B
    target := DontCare
    irst := false.B
    tlbrst := false.B

    ex := ExReq.mret
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

/**
  * Execution result of an retiring instruction
  * 
  * wb: writeback data
  * branch: branch info
  * mem: sequential memory access info
  *
  * @param coredef
  */
class RetireInfo(implicit val coredef: CoreDef) extends Bundle {
  val wb = UInt(coredef.XLEN.W)
  val branch = new BranchResult
  val hasMem = Bool()

  // TODO: put into PipeInstr
  def normalizedBranch(op: UInt, taken: Bool, npc: UInt): BranchResult = {
    val b = branch
    val result = Wire(new BranchResult)

    when(b.ex =/= ExReq.none) {
      result := b
    }.elsewhen(op === Decoder.Op("JAL").ident) {
      // assert(instr.forcePred)
      result.nofire()
    }.otherwise {
      when(b.branch === taken) {
        result.nofire()
      }.elsewhen(b.branch) {
        result := b
      }.otherwise {
        result.fire(npc)
      }
    }

    result
  }
}

object RetireInfo {
  def vacant(implicit coredef: CoreDef): RetireInfo = {
    val info = Wire(new RetireInfo)

    info.branch.nofire()
    info.wb := DontCare
    info.hasMem := false.B

    info
  }
}

/**
  * Instruction in execution pipeline, after both operands are ready
  * 
  * Besides the instruction from decoding, we have the following additional fields
  * - rs1val: value of the rs1 operand
  * - rs2val: value of the rs2 operand
  * - rdname: Name of the rd register. This comes from renaming
  * - tag: tag of this instruction. Tags are self-incrementing based on issue order, and wraps
  *   around at length(rob) = 2^length(name) = MAX_INFLIGHT_INSTR.
  *   When its execution finishes, this instruction is always put into rob[tag]
  *
  * @param coredef
  */
class PipeInstr(implicit val coredef: CoreDef) extends Bundle {
  val instr = new InstrExt

  val rs1val = UInt(coredef.XLEN.W)
  val rs2val = UInt(coredef.XLEN.W)

  val rdname = UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)
  val tag = UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)
}

/**
  * Instruction in reservation station
  * 
  * Besides the fields in PipeStr, we have the following additional fields
  * - rs1name: name of the rs1 operand
  * - rs2name: name of the rs2 operand
  * - rs1ready: is rs1 ready?
  * - rs2ready: is rs2 ready?
  *
  * @param coredef
  */
class ReservedInstr(override implicit val coredef: CoreDef) extends PipeInstr {
  val rs1name = UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)
  val rs2name = UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)
  val rs1ready = Bool()
  val rs2ready = Bool()

  def inval = instr.invalAddr || instr.instr.base === InstrType.RESERVED

  def ready = (inval || rs1ready && rs2ready)
}

/**
 * Instruction pushed by issuer, and reused by rob
 */
class InflightInstr(implicit val coredef: CoreDef) extends Bundle {
  // val rdname = UInt(coredef.XLEN.W)
  // rdname === tag, so we don't need this wire anymore
  val op = UInt(5.W)
  val isC = Bool()
  val addr = UInt(coredef.XLEN.W)
  val erd = UInt(log2Ceil(32).W) // Effective rd
  val pred = new BPUResult
  val forcePred = Bool() // FIXME: change to default pred

  def taken = pred.prediction === BHTPrediction.taken || forcePred
  def npc = Mux(isC, 2.U, 4.U) +% addr
}

object InflightInstr {
  def from(instr: InstrExt)(implicit coredef: CoreDef) = {
    val ret = Wire(new InflightInstr)
    ret.op := instr.instr.op
    ret.addr := instr.addr
    ret.isC := instr.instr.base === InstrType.C
    ret.erd := instr.instr.getRd()
    ret.pred := instr.pred
    ret.forcePred := instr.forcePred

    ret
  }
}

object PipeInstr {
  def empty(implicit coredef: CoreDef): PipeInstr = {
    val ret = Wire(new PipeInstr)
    ret.instr := InstrExt.empty
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
    ret.instr := InstrExt.empty

    ret
  }
}

/**
  * IO port of an execution unit
  * 
  * - next: next instruction
  * - stall: unit -> pipeline stall signal
  * - pause: pipeline -> unit stall signal
  * - flush: Flush signal
  * - retired: The instruction that just finished executing
  * - retirement: Result of execution
  *
  * @param coredef
  */
class ExecUnitPort(implicit val coredef: CoreDef) extends Bundle {
  val next = Input(new PipeInstr)

  val stall = Output(Bool())
  val flush = Input(Bool())

  val retirement = Output(new RetireInfo)
  val retired = Output(new PipeInstr)
}

/**
  * Trait repersenting an execution unit
  * 
  * DEPTH is the pipeline delay of this unit. For example, ALU, which works in and asynchronous manner,
  * have DEPTH = 0. LSU have a DEPTH of 1, and Div's DEPTH depends on its configuration
  */
trait ExecUnitInt {
  val DEPTH: Int
  val io: ExecUnitPort
}

/**
  * Base class of and execution unit
  *
  * This class automatically generates stage registers, which contains the instruction and an custom
  * bundle specified by the implementation.
  * 
  * Implementations are required to implement two methods:
  * - def map(stage: Int, pipe: PipeInstr, ext: Option[T]): (T, Bool)
  *   Mapping of one stage
  * - def finalize(pipe: PipeInstr, ext: T): RetireInfo 
  *   Mapping from the last stage's output into RetireInfo
  *
  * @param DEPTH: pipeline delay
  * @param ExtData: extra data's type
  * @param coredef: core defination
  */
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

      when(!io.stall) {
        current(0).pipe := io.next
        current(0).ext := fExt
      }

      for(i <- (1 until DEPTH)) {
        val (nExt, sStall) = connectStage(i, current(i-1).pipe, Some(current(i-1).ext))
        when(!io.stall) {
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

/**
  * Entry of common data bus
  * 
  * - valid: does this entry have any data in it?
  * - name: name of the broadcasted virtual register
  * - data: value to be broadcasted
  *
  * @param coredef core defination
  */
class CDBEntry(implicit val coredef: CoreDef) extends Bundle {
  val valid = Bool()
  val name = UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)
  val data = UInt(coredef.XLEN.W)
}

/**
  * Common data bus
  *
  * @param coredef core defination
  */
class CDB(implicit val coredef: CoreDef) extends Bundle {
  val entries = Vec(coredef.UNIT_COUNT+1, new CDBEntry)
}

/**
  * Additional ports
  */

class DelayedMemResult(implicit val coredef: CoreDef) extends Bundle {
  val isLoad = Bool()
  val data = UInt(coredef.XLEN.W)
}

trait WithCSRWriter {
  val writer: CSRWriter
}

trait WithPrivPort {
  val priv: PrivLevel.Type
}

trait WithStatus {
  val status: Status
}
