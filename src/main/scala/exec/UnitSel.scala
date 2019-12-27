package exec

import chisel3._
import _root_.core.CoreDef
import Chisel.experimental.chiselName
import chisel3.util.MuxCase
import instr.Instr
import exec.UnitSel.Retirement
import chisel3.util.log2Ceil
import _root_.core.CSRWriter
import scala.collection.mutable
import units.WithCSRWriter
import exec.units.WithLSUPort
import cache.DCReader
import _root_.core.ExType
import _root_.core.ExReq
import _root_.instr.Decoder
import _root_.core.Const

/**
 * Read instructions from reservation stations, and send them into (probably one of multiple) exec unit
 * 
 * All units are buffered into the same delay cycle count, so that we can assert only one
 * of them may retire an instr at within one cycle
 */
@chiselName
class UnitSel(
  gen : => Seq[ExecUnitInt],
  arbitration: Instr => Seq[Bool],
  bypassIdx: Option[Int] = None,
  hasPipe: Boolean = true
)(implicit val coredef: CoreDef) extends MultiIOModule {
  val units = gen

  val ctrl = IO(new Bundle {
    val stall = Output(Bool())
    // You may never intentionally pause an exec unit
    val flush = Input(Bool())
  })

  val rs = IO(Flipped(new ResStationExgress))

  val retire = IO(Output(new Retirement))

  // Extra ports
  val extras = new mutable.HashMap[String, Data]()
  for(u <- units) {
    if(u.isInstanceOf[WithCSRWriter]) {
      println("Found extra port: CSR")
      val csr = IO(new CSRWriter(coredef.XLEN))
      u.asInstanceOf[WithCSRWriter].writer <> csr
      extras.put("CSR", csr)
    }

    if(u.isInstanceOf[WithLSUPort]) {
      println("Found extra port: LSU")
      val dcReader = IO(new DCReader(coredef.L1D))
      val saUp = IO(Output(Bool()))
      u.asInstanceOf[WithLSUPort].reader <> dcReader
      saUp := u.asInstanceOf[WithLSUPort].saUp
      extras.put("LSU", dcReader)
      extras.put("saUp", saUp)
    }
  }

  val stall = false.B
  ctrl.stall := stall

  for(u <- units) {
    u.io.flush := ctrl.flush
    u.io.pause := false.B
  }

  // Arbitration
  for(u <- units) {
    u.io.next := PipeInstr.empty
  }

  val execMap = Wire(Vec(units.length, Bool()))
  execMap := arbitration(rs.instr.instr.instr)
  // Asserts exactly one can exec this instr

  // Contains a bypass unit, bypassing all inval instructions to there
  if(bypassIdx.isDefined) {
    when(rs.instr.inval) {
      execMap := VecInit(Seq.fill(units.length)(false.B))
      execMap(bypassIdx.get) := true.B
    }
  }

  val execMapUInt = execMap.asUInt
  val execMapNoDup = !((execMapUInt -% 1.U) & execMapUInt).orR
  assert(!rs.valid || execMapNoDup && execMapUInt.orR())

  val pipeExecMap = RegInit(VecInit(Seq.fill(units.length)(false.B)))
  val pipeInstr = RegInit(ReservedInstr.empty)
  val pipeInstrValid = RegInit(false.B)

  val pipeInput = Wire(Bool())

  rs.pop := false.B

  if(hasPipe) {
    when(pipeInstrValid) {
      pipeInput := false.B
      for((u, e) <- units.zip(pipeExecMap)) {
        when(e) {
          u.io.next := pipeInstr
          pipeInput := !u.io.stall
        }
      }
    }.otherwise {
      pipeInput := true.B
    }

    when(pipeInput) {
      pipeInstr := rs.instr
      pipeExecMap := execMap
      pipeInstrValid := rs.valid
      rs.pop := rs.valid
    }
  } else {
    pipeInput := DontCare
    when(rs.valid) {
      for((u, e) <- units.zip(execMap)) {
        when(e) {
          u.io.next := rs.instr
          rs.pop := !u.io.stall
        }
      }
    }
  }

  // Puts into retire FIFO 

  val maxDepth = units.map(_.DEPTH).max
  val noDelayUnitCount = units.count(_.DEPTH == 0)
  val fifoDepth = units.map(_.DEPTH).sum - maxDepth + units.size - noDelayUnitCount + 2

  // One extra cell for asserting retireTail never reaches retireHead

  if(units.length == 1) {
    println("UnitSel: Single unit")
    val pipeRetire = RegInit(Retirement.empty)
    retire := pipeRetire
    when(units(0).io.stall) {
      pipeRetire := Retirement.empty
    }.otherwise {
      pipeRetire := Retirement.from(units(0).io)
    }
    when(ctrl.flush) {
      pipeRetire := Retirement.empty
    }
  } else if(maxDepth == 0) {
    println("UnitSel: All units have 0 delay")
    val pipeRetire = RegInit(Retirement.empty)
    retire := pipeRetire
    pipeRetire := MuxCase(Retirement.empty, units.map(u => (
      !u.io.stall && !u.io.retired.instr.vacant, Retirement.from(u.io)
    )))

    when(ctrl.flush) {
      pipeRetire := Retirement.empty
    }
  } else {
    println(s"UnitSel: with FIFO depth $fifoDepth")

    val retireFifo = RegInit(VecInit(Seq.fill(fifoDepth)(Retirement.empty)))
    val retireHead = RegInit(0.U(log2Ceil(fifoDepth).W))
    val retireTail = RegInit(0.U(log2Ceil(fifoDepth).W))

    var prevTail = retireTail
    for(u <- units) {
      val newTail = Wire(UInt(log2Ceil(fifoDepth).W))
      newTail := prevTail
      when(!u.io.stall && !u.io.retired.instr.vacant) {
        retireFifo(prevTail) := Retirement.from(u.io)
        newTail := Mux(prevTail === (fifoDepth-1).U, 0.U, prevTail +% 1.U)
      }

      assert(prevTail === retireHead || newTail =/= retireHead)
      prevTail = newTail
    }
    retireTail := prevTail

    // Output

    when(retireTail === retireHead) {
      retire := Retirement.empty
    }.otherwise {
      retire := retireFifo(retireHead)
      retireHead := Mux(retireHead === (fifoDepth-1).U, 0.U, retireHead +% 1.U)
    }

    when(ctrl.flush) {
      retireHead := 0.U
      retireTail := 0.U
    }
  }

  // Flush
  when(ctrl.flush) {
    pipeInstrValid := false.B
  }
}

object UnitSel {
  class Retirement(implicit val coredef: CoreDef)  extends Bundle {
    val instr = new PipeInstr
    val info = new RetireInfo

    def normalizedBranch(): BranchResult = {
      val b = info.branch
      val result = Wire(new BranchResult)

      when(b.ex =/= ExReq.none) {
        result := b
      }.elsewhen(instr.instr.instr.op === Decoder.Op("JAL").ident) {
        result.nofire()
      }.elsewhen(instr.instr.instr.op =/= Decoder.Op("BRANCH").ident) {
        result := b
      }.otherwise {
        when(b.branch === instr.instr.branchPred) {
          result.nofire()
        }.elsewhen(b.branch) {
          result := b
        }.otherwise {
          result.fire(instr.instr.npc)
        }
      }

      result
    }
  }

  object Retirement {
    def empty(implicit coredef: CoreDef): Retirement = {
      val ret = Wire(new Retirement)
      ret.instr := PipeInstr.empty
      ret.info := RetireInfo.vacant

      ret
    }

    def from(port: ExecUnitPort)(implicit coredef: CoreDef): Retirement = {
      val ret = Wire(new Retirement)
      ret.instr := port.retired
      ret.info := port.retirement

      ret
    }
  }
}
