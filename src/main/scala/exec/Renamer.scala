package exec

import chisel3._
import _root_.core.CoreDef
import reg.RegWriter
import chisel3.util.log2Ceil
import instr.InstrExt
import Chisel.experimental.chiselName
import reg.RegReader
import chisel3.util.MuxLookup

@chiselName
class Renamer(implicit coredef: CoreDef) extends MultiIOModule {
  val REG_NUM = 32 // TODO: do we need to make this configurable?

  val cdb = IO(Input(new CDB))
  val rr = IO(Vec(coredef.ISSUE_NUM, Vec(2, new RegReader(coredef.XLEN))))

  val toExec = IO(new Bundle {
    val input = Input(Vec(coredef.ISSUE_NUM, new InstrExt))
    val commit = Input(UInt(log2Ceil(coredef.ISSUE_NUM+1).W))
    val ntag = Input(UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)) // Next tag also used for next rdname
    val output = Output(Vec(coredef.ISSUE_NUM, new ReservedInstr))
    val count = Output(UInt(log2Ceil(coredef.ISSUE_NUM+1).W))

    val flush = Input(Bool())
  })

  val NAME_LENGTH = log2Ceil(coredef.INFLIGHT_INSTR_LIMIT)
  val REG_ADDR_LENGTH = log2Ceil(REG_NUM)

  // Register <-> name mapping
  // If a register is unmapped, it implies that at least INFLIGHT_INSTR_LIMIT instruction
  // has been retired after the instruction which wrote to the register.
  // So it's value must have been stored into the regfile
  val reg2name = RegInit(VecInit(Seq.fill(REG_NUM)(0.U(NAME_LENGTH.W))))
  val name2reg = RegInit(VecInit(Seq.fill(coredef.INFLIGHT_INSTR_LIMIT)(0.U(log2Ceil(REG_NUM).W))))
  val regMapped = RegInit(VecInit(Seq.fill(REG_NUM)(false.B)))

  // Is the value for a specific name already broadcasted on the CDB?
  val nameReady = RegInit(VecInit(Seq.fill(coredef.INFLIGHT_INSTR_LIMIT)(true.B)))

  def flush() = {
    reg2name := VecInit(Seq.fill(REG_NUM)(0.U(NAME_LENGTH.W)))
    name2reg:= VecInit(Seq.fill(coredef.INFLIGHT_INSTR_LIMIT)(0.U(log2Ceil(REG_NUM).W)))
    nameReady := VecInit(Seq.fill(coredef.INFLIGHT_INSTR_LIMIT)(true.B))
    regMapped := VecInit(Seq.fill(REG_NUM)(false.B))
  }

  // Storage for name -> value
  // By default, all reg is unmapped
  val store = RegInit(VecInit(Seq.fill(coredef.INFLIGHT_INSTR_LIMIT)(0.U(coredef.XLEN.W))))

  // Update by CDB
  for(ent <- cdb.entries) {
    when(ent.valid) {
      nameReady(ent.name) := true.B
      store(ent.name) := ent.data
    }
  }

  // TODO: asserts that CDB never broadcasts names that are being allocated

  // Stage 1, allocate rd name, lookup register names
  val tags = (0 until coredef.ISSUE_NUM).map(idx => toExec.ntag +% idx.U)

  val canRename = (0 until coredef.ISSUE_NUM).map(idx => {
    if(idx == 0) {
      true.B
    } else {
      val ret = WireDefault(true.B)
      for(i <- (0 until idx)) {
        when(toExec.input(idx).instr.rs1 === toExec.input(i).instr.rd) {
          ret := false.B
        }

        when(toExec.input(idx).instr.rs2 === toExec.input(i).instr.rd) {
          ret := false.B
        }
      }
      ret
    }
  })

  def readRegs(r: RegReader, reg: UInt) = {
    r.addr := reg

    val name = reg2name(reg)
    val ready = WireDefault(nameReady(name))
    val value = WireDefault(store(name))

    // Loop through CDB
    for(ent <- cdb.entries) {
      when(ent.valid && ent.name === name) {
        ready := true.B
        value := ent.data
      }
    }

    when(!regMapped(reg)) {
      ready := true.B
      value := r.data
    }

    when(reg === 0.U) {
      ready := true.B
      value := 0.U
    }

    (name, ready, value)
  }

  toExec.count := MuxLookup(
    false.B,
    coredef.ISSUE_NUM.U,
    canRename.zipWithIndex.map({ case (bit, idx) => bit -> idx.U })
  )

  for((instr, idx) <- toExec.input.zipWithIndex) {
    val (rs1name, rs1ready, rs1val) = readRegs(rr(idx)(0), instr.instr.rs1)
    val (rs2name, rs2ready, rs2val) = readRegs(rr(idx)(1), instr.instr.rs2)

    toExec.output(idx).rs1name := rs1name
    toExec.output(idx).rs2name := rs2name

    toExec.output(idx).rs1ready := rs1ready
    toExec.output(idx).rs2ready := rs2ready

    toExec.output(idx).rs1val := rs1val
    toExec.output(idx).rs2val := rs2val

    toExec.output(idx).instr := instr
    toExec.output(idx).rdname := tags(idx)
    toExec.output(idx).tag := tags(idx)

    when(idx.U < toExec.commit) {
      val original = name2reg(tags(idx))
      reg2name(instr.instr.rd) := tags(idx)
      name2reg(tags(idx)) := instr.instr.rd
      // TODO: use retirement rd to unset. Then we can remove name2reg mapping entirely
      if(idx == 1) {
        when(reg2name(original) === tags(idx) && original =/= toExec.input(0).instr.rd) {
          regMapped(original) := false.B
        }
      } else {
        when(reg2name(original) === tags(idx)) {
          regMapped(original) := false.B
        }
      }
      regMapped(instr.instr.rd) := true.B
      nameReady(tags(idx)) := false.B
    }
  }

  // Lastly, handles flush
  when(toExec.flush) {
    flush()
  }
}
