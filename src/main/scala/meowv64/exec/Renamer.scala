package meowv64.exec

import chisel3._
import meowv64.core.CoreDef
import meowv64.reg.RegWriter
import chisel3.util.log2Ceil
import meowv64.instr.InstrExt
import Chisel.experimental.chiselName
import meowv64.reg.RegReader
import chisel3.util.MuxLookup

class Release(implicit val coredef: CoreDef) extends Bundle {
  val name = Input(UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W))
  val reg = Input(UInt(log2Ceil(32).W))
  val value = Output(UInt(coredef.XLEN.W))
}

@chiselName
class Renamer(implicit coredef: CoreDef) extends MultiIOModule {
  val REG_NUM = 32 // TODO: do we need to make this configurable?

  val cdb = IO(Input(new CDB))
  val rr = IO(Vec(coredef.ISSUE_NUM, Vec(2, new RegReader(coredef.XLEN))))
  val rw = IO(Vec(coredef.ISSUE_NUM, new RegWriter(coredef.XLEN)))

  val toExec = IO(new Bundle {
    val input = Input(Vec(coredef.ISSUE_NUM, new InstrExt))
    val commit = Input(UInt(log2Ceil(coredef.ISSUE_NUM + 1).W))
    val ntag = Input(
      UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)
    ) // Next tag also used for next rdname
    val output = Output(Vec(coredef.ISSUE_NUM, new ReservedInstr))
    val allowBit = Output(Vec(coredef.ISSUE_NUM, Bool()))

    val releases = Vec(coredef.RETIRE_NUM, new Release)
    val retire = Input(UInt(log2Ceil(coredef.RETIRE_NUM + 1).W))

    val flush = Input(Bool())
  })

  val NAME_LENGTH = log2Ceil(coredef.INFLIGHT_INSTR_LIMIT)
  val REG_ADDR_LENGTH = log2Ceil(REG_NUM)

  // Register <-> name mapping
  // If a register is unmapped, it implies that at least INFLIGHT_INSTR_LIMIT instruction
  // has been retired after the instruction which wrote to the register.
  // So it's value must have been stored into the regfile
  val reg2name = RegInit(VecInit(Seq.fill(REG_NUM)(0.U(NAME_LENGTH.W))))
  val regMapped = RegInit(VecInit(Seq.fill(REG_NUM)(false.B)))

  // Is the value for a specific name already broadcasted on the CDB?
  val nameReady = RegInit(
    VecInit(Seq.fill(coredef.INFLIGHT_INSTR_LIMIT)(true.B))
  )

  def flush() = {
    reg2name := VecInit(Seq.fill(REG_NUM)(0.U(NAME_LENGTH.W)))
    nameReady := VecInit(Seq.fill(coredef.INFLIGHT_INSTR_LIMIT)(true.B))
    regMapped := VecInit(Seq.fill(REG_NUM)(false.B))
  }

  // Storage for name -> value
  // By default, all reg is unmapped
  val store = RegInit(
    VecInit(Seq.fill(coredef.INFLIGHT_INSTR_LIMIT)(0.U(coredef.XLEN.W)))
  )

  // Update by CDB
  for (ent <- cdb.entries) {
    when(ent.valid) {
      nameReady(ent.name) := true.B
      store(ent.name) := ent.data
    }
  }

  // TODO: asserts that CDB never broadcasts names that are being allocated

  val tags = (0 until coredef.ISSUE_NUM).map(idx => toExec.ntag +% idx.U)

  val canRename = (0 until coredef.ISSUE_NUM).map(idx => {
    if (idx == 0) {
      true.B
    } else {
      val ret = WireDefault(true.B)
      for (i <- (0 until idx)) {
        when(toExec.input(idx).instr.getRs1 === toExec.input(i).instr.getRd) {
          ret := false.B
        }

        when(toExec.input(idx).instr.getRs2 === toExec.input(i).instr.getRd) {
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
    for (ent <- cdb.entries) {
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

  toExec.allowBit := VecInit(canRename)

  // Release before allocation
  for ((release, idx) <- toExec.releases.zipWithIndex) {
    val data = store(release.name)
    when(idx.U < toExec.retire) {
      regMapped(release.reg) := reg2name(release.reg) =/= release.name
      rw(idx).addr := release.reg
      rw(idx).data := data
    }.otherwise {
      rw(idx).addr := 0.U
      rw(idx).data := DontCare
    }
    release.value := data
  }

  for ((instr, idx) <- toExec.input.zipWithIndex) {
    val (rs1name, rs1ready, rs1val) = readRegs(rr(idx)(0), instr.instr.getRs1)
    val (rs2name, rs2ready, rs2val) = readRegs(rr(idx)(1), instr.instr.getRs2)

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
      reg2name(instr.instr.getRd) := tags(idx)
      regMapped(instr.instr.getRd) := true.B
      nameReady(tags(idx)) := false.B
    }
  }

  // Lastly, handles flush
  when(toExec.flush) {
    flush()
  }
}
