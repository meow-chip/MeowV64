package exec

import chisel3._
import _root_.core.CoreDef
import reg.RegWriter
import chisel3.util.log2Ceil
import instr.InstrExt
import Chisel.experimental.chiselName
import reg.RegReader

@chiselName
class Renamer(implicit coredef: CoreDef) extends MultiIOModule {
  val REG_NUM = 32 // TODO: do we need to make this configurable?

  val cdb = IO(Input(new CDB))
  val rr = IO(Vec(coredef.ISSUE_NUM * 2, new RegReader(coredef.XLEN)))

  val toExec = IO(new Bundle {
    val input = Input(Vec(coredef.ISSUE_NUM, new InstrExt(coredef.ADDR_WIDTH)))
    val commit = Input(UInt(log2Ceil(coredef.ISSUE_NUM+1).W))
    val ntag = Input(UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W))
    val output = Output(Vec(coredef.ISSUE_NUM, new ReservedInstr))

    val invalMap = Input(Vec(coredef.ISSUE_NUM, Bool()))

    val flush = Input(Bool())
  })

  for(o <- toExec.output) {
    // Apply assertions
    o.validate()
  }

  val NAME_LENGTH = log2Ceil(coredef.INFLIGHT_INSTR_LIMIT)
  val REG_ADDR_LENGTH = log2Ceil(REG_NUM)

  // Reg -> Name map
  class State extends Bundle {
    // Register <-> name mapping
    // If a register is mapped to name 0, it implies that at least INFLIGHT_INSTR_LIMIT instruction
    // has been retired after the instruction which wrote to the register.
    // So it's value must have been stored into the regfile
    val reg2name = Vec(REG_NUM, UInt(NAME_LENGTH.W))
    val name2reg = Vec(coredef.INFLIGHT_INSTR_LIMIT, UInt(log2Ceil(REG_NUM).W))

    // TODO: assert: bidirectional mapping perserved

    // Is the value for a specific name already broadcasted on the CDB?
    val nameReady = Vec(coredef.INFLIGHT_INSTR_LIMIT, Bool())

    // Next name
    val nextUp = UInt(NAME_LENGTH.W)

    def validate() {
      // Reg 0 is always mapped to name 0
      assert(reg2name(0) === 0.U)
      assert(name2reg(0) === 0.U)

      // Name 0 always have a ready value
      assert(nameReady(0))
    }
  }

  object State {
    def default(): State = {
      val ret = Wire(new State)

      ret.reg2name := VecInit(Seq.fill(REG_NUM)(0.U(NAME_LENGTH.W)))
      ret.name2reg:= VecInit(Seq.fill(coredef.INFLIGHT_INSTR_LIMIT)(0.U(log2Ceil(REG_NUM).W)))
      ret.nameReady := VecInit(Seq.fill(coredef.INFLIGHT_INSTR_LIMIT)(true.B))
      ret.nextUp := 1.U

      ret
    }
  }

  // Storage for name -> value
  // By default, all reg is ready and mapped to name 0, which always has a value of 0
  val store = RegInit(VecInit(Seq.fill(coredef.INFLIGHT_INSTR_LIMIT)(0.U(coredef.XLEN.W))))
  val state = RegInit(State.default())
  state.validate()
  assert(store(0) === 0.U)

  // Mask with current CDB request
  var modState = Wire(new State())
  var modStore = Wire(Vec(coredef.INFLIGHT_INSTR_LIMIT, UInt(coredef.XLEN.W)))
  modState := state
  modStore := store

  modState.validate()

  for(ent <- cdb.entries) {
    when(ent.valid && ent.name =/= 0.U) {
      modState.nameReady(ent.name) := true.B
      modStore(ent.name) := ent.data
    }
  }

  // By default, update state by modState
  state := modState
  store := modStore

  // If there are issues, additional modification are applied
  var prev = modState
  for(((row, out), idx) <- toExec.input.zip(toExec.output).zipWithIndex) {
    val next = Wire(new State)
    next.validate()

    next := prev

    // Assign instr and tag
    out.instr := row
    out.tag := toExec.ntag +% idx.U

    // Rename rd
    val rd = row.instr.getRd
    when(row.vacant || rd === 0.U || toExec.invalMap(idx)) {
      // Next kept unchanged
      out.rdname := 0.U
    }.otherwise {
      val prevName = prev.reg2name(rd)
      val prevReg = prev.name2reg(prev.nextUp)
      // Unbinds first
      when(prevReg =/= rd) {
        next.reg2name(prevReg) := 0.U
      }
      when(prevName =/= prev.nextUp) {
        next.name2reg(prevName) := 0.U
      }
      // Bind new
      next.reg2name(rd) := prev.nextUp
      next.name2reg(prev.nextUp) := rd

      next.nextUp := Mux(prev.nextUp === (coredef.INFLIGHT_INSTR_LIMIT-1).U, 1.U, prev.nextUp +% 1.U)
      next.nameReady(prev.nextUp) := false.B

      out.rdname := prev.nextUp
    }

    // Read rs
    val rs1 = row.instr.getRs1
    val rs2 = row.instr.getRs2

    rr(idx*2).addr := rs1
    rr(idx*2+1).addr := rs2

    // Name, ready, val
    def readReg(s: State, store: Vec[UInt], addr: UInt, rr: RegReader): (UInt, Bool, UInt) = {
      rr.addr := addr

      val name = s.reg2name(addr)
      val ready = s.nameReady(name) // For name = 0, nameReady(0) is always asserted
      val data = Mux(name === 0.U, rr.data, store(name))

      (name, ready, data)
    }

    val (rs1name, rs1ready, rs1val) = readReg(prev, modStore, rs1, rr(idx * 2))
    val (rs2name, rs2ready, rs2val) = readReg(prev, modStore, rs2, rr(idx * 2 + 1))

    out.rs1name := rs1name
    out.rs1ready := rs1ready || toExec.invalMap(idx)
    out.rs1val := rs1val

    out.rs2name := rs2name
    out.rs2ready := rs2ready || toExec.invalMap(idx)
    out.rs2val := rs2val

    when(toExec.commit === (idx+1).U) {
      state := next
    }

    prev = next
  }

  // Lastly, handles flush
  when(toExec.flush) {
    state := State.default()
  }
}
