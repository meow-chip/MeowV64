package meowv64.exec

import chisel3._
import chisel3.util._
import meowv64.core.CoreDef
import meowv64.instr.InstrExt
import meowv64.reg.RegReader
import meowv64.reg.RegWriter
import meowv64.instr.RegIndex

class Release(implicit val coredef: CoreDef) extends Bundle {
  val name = Input(UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W))
  val reg = Input(new RegIndex())
  val value = Output(UInt(coredef.XLEN.W))
}

class Renamer(implicit coredef: CoreDef) extends Module {
  val REG_NUM = 32 // TODO: do we need to make this configurable?

  val cdb = IO(Input(new CDB))

  val ports =
    IO(MixedVec(for ((ty, width) <- coredef.REGISTERS_TYPES) yield new Bundle {
      // each instruction read two registers
      val rr = Vec(coredef.ISSUE_NUM, Vec(2, new RegReader(width)))
      // each instruction write to one register
      val rw = Vec(coredef.ISSUE_NUM, new RegWriter(width))
      rr.suggestName(s"rr_${ty}")
      rw.suggestName(s"rw_${ty}")
    }))

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

  val banks = for ((ty, width) <- coredef.REGISTERS_TYPES) yield new {
    // Register <-> name mapping
    // If a register is unmapped, it implies that at least INFLIGHT_INSTR_LIMIT instruction
    // has been retired after the instruction which wrote to the register.
    // So it's value must have been stored into the regfile
    val reg2name = RegInit(VecInit(Seq.fill(REG_NUM)(0.U(NAME_LENGTH.W))))
    val regMapped = RegInit(VecInit(Seq.fill(REG_NUM)(false.B)))
  }

  // Is the value for a specific name already broadcasted on the CDB?
  val nameReady = RegInit(
    VecInit(Seq.fill(coredef.INFLIGHT_INSTR_LIMIT)(true.B))
  )

  // Storage for name -> value
  // By default, all reg is unmapped
  val store = RegInit(
    VecInit(Seq.fill(coredef.INFLIGHT_INSTR_LIMIT)(0.U(coredef.XLEN.W)))
  )

  def flush() = {
    for (bank <- banks) {
      bank.reg2name := VecInit(Seq.fill(REG_NUM)(0.U(NAME_LENGTH.W)))
      bank.regMapped := VecInit(Seq.fill(REG_NUM)(false.B))
    }
    nameReady := VecInit(Seq.fill(coredef.INFLIGHT_INSTR_LIMIT)(true.B))
  }

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
      val rs1 = toExec.input(idx).instr.getRs1
      val rs2 = toExec.input(idx).instr.getRs2

      // check if this instruction relies on previous instructions
      for (i <- (0 until idx)) {
        val rd = toExec.input(i).instr.getRd
        when(rs1.ty === rd.ty && rs1.index === rd.index) {
          ret := false.B
        }

        when(rs2.ty === rd.ty && rs2.index === rd.index) {
          ret := false.B
        }
      }
      ret
    }
  })

  def readRegs(r: RegReader, reg: UInt, bankIdx: Int) = {
    r.addr := reg

    val reg2name = banks(bankIdx).reg2name
    val regMapped = banks(bankIdx).regMapped

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
      for ((bank, (ty, _)) <- banks.zip(coredef.REGISTERS_TYPES)) {
        val reg2name = bank.reg2name
        val regMapped = bank.regMapped
        when(release.reg.ty === ty) {
          regMapped(release.reg.index) := reg2name(
            release.reg.index
          ) =/= release.name
        }
      }

      // TODO: check register type
      for (((ty, _), i) <- coredef.REGISTERS_TYPES.zipWithIndex) {
        val rw = ports(i).rw
        rw(idx).addr := 0.U
        rw(idx).data := DontCare

        when(release.reg.ty === ty) {
          rw(idx).addr := release.reg.index
          rw(idx).data := data
        }
      }
    }.otherwise {
      for (i <- 0 until coredef.REGISTERS_TYPES.length) {
        val rw = ports(i).rw
        rw(idx).addr := 0.U
        rw(idx).data := DontCare
      }
    }
    release.value := data
  }

  for ((instr, idx) <- toExec.input.zipWithIndex) {
    toExec.output(idx).rs1name := 0.U
    toExec.output(idx).rs1ready := false.B
    toExec.output(idx).rs1val := 0.U
    toExec.output(idx).rs2name := 0.U
    toExec.output(idx).rs2ready := false.B
    toExec.output(idx).rs2val := 0.U

    for (((ty, _), bankIdx) <- coredef.REGISTERS_TYPES.zipWithIndex) {
      val rr = ports(bankIdx).rr
      rr(idx)(0).addr := 0.U
      rr(idx)(1).addr := 0.U

      when(instr.instr.getRs1Type === ty) {
        val (rs1name, rs1ready, rs1val) =
          readRegs(rr(idx)(0), instr.instr.getRs1Index, bankIdx)
        toExec.output(idx).rs1name := rs1name
        toExec.output(idx).rs1ready := rs1ready
        toExec.output(idx).rs1val := rs1val
      }

      when(instr.instr.getRs2Type === ty) {
        val (rs2name, rs2ready, rs2val) =
          readRegs(rr(idx)(1), instr.instr.getRs2Index, bankIdx)
        toExec.output(idx).rs2name := rs2name
        toExec.output(idx).rs2ready := rs2ready
        toExec.output(idx).rs2val := rs2val
      }
    }

    toExec.output(idx).instr := instr
    toExec.output(idx).rdname := tags(idx)
    toExec.output(idx).tag := tags(idx)

    when(idx.U < toExec.commit) {
      for (((ty, _), bank) <- coredef.REGISTERS_TYPES.zip(banks)) {
        when(instr.instr.getRdType() === ty) {
          bank.reg2name(instr.instr.getRdIndex) := tags(idx)
          bank.regMapped(instr.instr.getRdIndex) := true.B
        }
      }

      nameReady(tags(idx)) := false.B
    }
  }

  // Lastly, handles flush
  when(toExec.flush) {
    flush()
  }
}
