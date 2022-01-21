package meowv64.exec

import chisel3.Module
import chisel3._
import chisel3.util._
import chisel3.util.log2Ceil
import meowv64.core.CSRWriter
import meowv64.core.CoreDef
import meowv64.core.ExReq
import meowv64.core.ExType
import meowv64.core.PrivLevel
import meowv64.core.Status
import meowv64.instr.BHTPrediction
import meowv64.instr.BPUResult
import meowv64.instr.Decoder.InstrType
import meowv64.instr.InstrExt
import meowv64.instr.RegIndex

/** Branch result
  *
  * Branch are used to state an interrupt in pipelined execution, which comes
  * from the execution of an instruction. Following conditions are considered as
  * a branch:
  *   - Jumps and branches that don't match branch prediction results
  *   - Instructions caused an exceptions
  *   - Instructions have side-effects on previous stages, so that we need to
  *     flush the pipeline. This includes:
  *     - CSR writes (may alter instr fetch)
  *     - FENCE.I
  *
  * @param coredef:
  *   Core definition
  */
class BranchResult(implicit val coredef: CoreDef) extends Bundle {

  /** Whether a pipeline branch occurs
    */
  val branch = Bool()

  /** ICache Flush for fence.i
    */
  val iRst = Bool()

  /** TLB Flush for sfence.vma
    */
  val tlbRst = Bool()

  /** Target PC
    */
  val target = UInt(coredef.XLEN.W)

  val ex = ExReq()
  val exType = ExType()

  def nofire = {
    branch := false.B
    target := 0.U
    iRst := false.B
    tlbRst := false.B

    ex := ExReq.none
    exType := DontCare
  }

  def fire(addr: UInt) = {
    branch := true.B
    target := addr
    iRst := false.B
    tlbRst := false.B

    ex := ExReq.none
    exType := DontCare
  }

  def ifence(addr: UInt) = {
    branch := true.B
    target := addr
    iRst := true.B
    tlbRst := false.B

    ex := ExReq.none
    exType := DontCare
  }

  def sfence(addr: UInt) = {
    branch := true.B
    target := addr
    iRst := false.B
    tlbRst := true.B

    ex := ExReq.none
    exType := DontCare
  }

  def ex(et: ExType.Type) {
    branch := false.B
    target := DontCare
    iRst := false.B
    tlbRst := false.B

    ex := ExReq.ex
    exType := et
  }

  def ret(req: ExReq.Type) {
    branch := false.B
    target := DontCare
    iRst := false.B
    tlbRst := false.B

    ex := req
    exType := DontCare
  }

  def branched(): Bool = branch || ex =/= ExReq.none
}

object BranchResult {
  def empty(implicit coredef: CoreDef): BranchResult = {
    val ret = Wire(new BranchResult)
    ret.nofire
    ret
  }
}

/** Execution result of an retiring instruction
  *
  * wb: writeback data branch: branch info mem: sequential memory access info
  *
  * @param coredef
  */
class RetireInfo(implicit val coredef: CoreDef) extends Bundle {

  /** Writeback data or trap value(mtval, stval)
    */
  val wb = UInt(coredef.XLEN.W)

  /** Update fflags for floating point operations
    */
  val updateFFlags = Bool()
  val fflags = UInt(5.W)

  /** Generic branch result
    */
  val branch = new BranchResult
  val hasMem = Bool()

  /** Whether this branch has taken. Used in updating BPU.
    */
  val branchTaken = Bool()
}

object RetireInfo {
  def vacant(implicit coredef: CoreDef): RetireInfo = {
    val info = Wire(new RetireInfo)

    info.branch.nofire
    info.wb := 0.U
    info.hasMem := false.B
    info.branchTaken := false.B
    info.updateFFlags := false.B
    info.fflags := 0.U

    info
  }
}

/** Instruction in execution pipeline, after both operands are ready
  *
  * Besides the instruction from decoding, we have the following additional
  * fields
  *   - rs1val: value of the rs1 operand
  *   - rs2val: value of the rs2 operand
  *   - rs3val: value of the rs3 operand
  *   - rdname: Name of the rd register. This comes from renaming
  *   - tag: tag of this instruction. Tags are self-incrementing based on issue
  *     order, and wraps around at length(rob) = 2^length(name) =
  *     MAX_INFLIGHT_INSTR. When its execution finishes, this instruction is
  *     always put into rob[tag]
  *
  * @param coredef
  */
class PipeInstr(implicit val coredef: CoreDef) extends Bundle {
  val instr = new InstrExt

  val rs1val = UInt(coredef.XLEN.W)
  val rs2val = UInt(coredef.XLEN.W)
  val rs3val = UInt(coredef.XLEN.W)

  val rdname = UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)
  val tag = UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)
}

/** Instruction in reservation station
  *
  * Besides the fields in PipeStr, we have the following additional fields
  *   - rs1name: name of the rs1 operand
  *   - rs2name: name of the rs2 operand
  *   - rs3name: name of the rs3 operand
  *   - rs1ready: is rs1 ready?
  *   - rs2ready: is rs2 ready?
  *   - rs3ready: is rs3 ready?
  *
  * @param coredef
  */
class ReservedInstr(override implicit val coredef: CoreDef) extends PipeInstr {
  val rs1name = UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)
  val rs2name = UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)
  val rs3name = UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)
  val rs1ready = Bool()
  val rs2ready = Bool()
  val rs3ready = Bool()

  /** Illegal instruction
    */
  def illegal = instr.illegal

  def ready = (illegal || (rs1ready && rs2ready && rs3ready))
}

/** Instruction pushed by issuer, and reused by rob
  */
class InflightInstr(implicit val coredef: CoreDef) extends Bundle {
  // val rdname = UInt(coredef.XLEN.W)
  // rdname === tag, so we don't need this wire anymore
  val op = UInt(5.W)
  val isC = Bool()
  val addr = UInt(coredef.XLEN.W)
  val erd = new RegIndex() // Effective rd
  /** Prediction result from BPU
    */
  val pred = new BPUResult

  /** Override prediction result to be taken e.g. JAL
    */
  val overridePred = Bool() // FIXME: change to default pred

  def taken = overridePred || pred.prediction === BHTPrediction.taken
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
    ret.overridePred := instr.overridePred

    ret
  }
}

object PipeInstr {
  def empty(implicit coredef: CoreDef): PipeInstr = {
    val ret = Wire(new PipeInstr)
    ret.instr := InstrExt.empty
    ret.rs1val := DontCare
    ret.rs2val := DontCare
    ret.rs3val := DontCare
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

/** IO port of an execution unit
  *
  *   - next: next instruction
  *   - stall: unit -> pipeline stall signal
  *   - flush: Flush signal
  *   - retired: The instruction that just finished executing
  *   - retirement: Result of execution
  *
  * @param coredef
  */
class ExecUnitPort(implicit val coredef: CoreDef) extends Bundle {
  val next = Input(new PipeInstr)

  val stall = Output(Bool())
  val flush = Input(Bool())

  /** The instruction that just finished execution
    */
  val retirement = Output(new RetireInfo)

  /** Result of execution
    */
  val retired = Output(new PipeInstr)
}

/** Trait representing an execution unit
  *
  * DEPTH is the pipeline delay of this unit. For example, ALU, which works in
  * and asynchronous manner, have DEPTH = 0. LSU have a DEPTH of 1, and Div's
  * DEPTH depends on its configuration
  */
trait ExecUnitInt {
  val DEPTH: Int
  val io: ExecUnitPort
}

/** Base class of an execution unit
  *
  * This class automatically generates stage registers, which contains the
  * instruction and an custom bundle specified by the implementation.
  *
  * Implementations are required to implement two methods:
  *   - def map(stage: Int, pipe: PipeInstr, ext: Option[T]): (T, Bool) Mapping
  *     of one stage
  *   - def finalize(pipe: PipeInstr, ext: T): RetireInfo Mapping from the last
  *     stage's output into RetireInfo
  *
  * @param DEPTH:
  *   pipeline delay
  * @param ExtData:
  *   extra data's type
  * @param coredef:
  *   core defination
  */
abstract class ExecUnit[T <: Data](
    val DEPTH: Int,
    val ExtData: T
)(implicit
    val coredef: CoreDef
) extends Module
    with ExecUnitInt {
  val io = IO(new ExecUnitPort)

  var current = if (DEPTH != 0) {
    val storeInit = Wire(
      Vec(
        DEPTH,
        new Bundle {
          val pipe = new PipeInstr
          val ext = ExtData.cloneType
        }
      )
    )

    // default wiring
    for (i <- (0 until DEPTH)) {
      storeInit(i) := DontCare
      // storeInit(i).pipe.instr.instr.imm := 0.S // Treadle bug?
      storeInit(i).pipe.instr.valid := false.B
      storeInit(i).ext := DontCare
    }

    RegInit(storeInit)
  } else {
    null
  }

  def init(): Unit = {
    if (DEPTH != 0) {
      // if any stage stall, the whole pipeline stalls
      val (fExt, fStall) = connectStage(0, io.next, None)
      var stall = fStall

      when(!io.stall) {
        current(0).pipe := io.next
        current(0).ext := fExt
      }

      for (i <- (1 until DEPTH)) {
        val (nExt, sStall) =
          connectStage(i, current(i - 1).pipe, Some(current(i - 1).ext))
        when(!io.stall) {
          current(i).pipe := current(i - 1).pipe
          current(i).ext := nExt
        }

        when(sStall) {
          current(i - 1).ext := nExt
        }

        stall = stall || sStall
      }

      val (nExt, lStall) = connectStage(
        DEPTH,
        current(DEPTH - 1).pipe,
        Some(current(DEPTH - 1).ext)
      )

      when(lStall) {
        current(DEPTH - 1).ext := nExt
      }

      when(io.flush) { // Override current
        for (c <- current) {
          c.pipe := PipeInstr.empty
          c.ext := DontCare
        }
      }

      io.retired := current(DEPTH - 1).pipe
      when(!io.retired.instr.valid) {
        io.retirement := RetireInfo.vacant
      }.otherwise {
        io.retirement := finalize(current(DEPTH - 1).pipe, nExt)
      }
      io.stall := stall || lStall
    } else {
      val (nExt, sStall) = connectStage(0, io.next, None)
      // Use chisel's unconnected wire check to enforce that no ext is exported from this exec unit
      io.retired := io.next
      when(!io.retired.instr.valid) {
        io.retirement := RetireInfo.vacant
      }.otherwise {
        io.retirement := finalize(io.next, nExt)
      }
      io.stall := sStall
    }
  }

  /** Override this function and define combinatorial logic
    */
  def map(stage: Int, pipe: PipeInstr, ext: Option[T]): (T, Bool)

  def finalize(pipe: PipeInstr, ext: T): RetireInfo

  def connectStage(stage: Int, pipe: PipeInstr, ext: Option[T]): (T, Bool) = {
    val nExt = Wire(ExtData.cloneType)
    val sStall = Wire(Bool())

    when(!pipe.instr.valid) {
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

/** Entry of common data bus
  *
  *   - valid: does this entry have any data in it?
  *   - name: name of the broadcasted virtual register
  *   - data: value to be broadcasted
  *
  * @param coredef
  *   core defination
  */
class CDBEntry(implicit val coredef: CoreDef) extends Bundle {
  val valid = Bool()
  val name = UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)
  val data = UInt(coredef.XLEN.W)
}

/** Common data bus
  *
  * @param coredef
  *   core defination
  */
class CDB(implicit val coredef: CoreDef) extends Bundle {
  val entries = Vec(coredef.UNIT_COUNT + 1, new CDBEntry)
}

/** Additional ports
  */

class DelayedMemResult(implicit val coredef: CoreDef) extends Bundle {
  val hasWB = Bool()
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
