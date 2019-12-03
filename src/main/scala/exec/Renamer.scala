package exec

import chisel3._
import _root_.core.CoreDef
import reg.RegWriter
import chisel3.util.log2Ceil
import instr.InstrExt
import Chisel.experimental.chiselName

@chiselName
class Renamer(implicit coredef: CoreDef) extends MultiIOModule {
  val cdb = IO(Input(new CDB))
  val rw = IO(Vec(coredef.RETIRE_NUM, new RegWriter(coredef.XLEN)))
  val rr = IO(Vec(coredef.ISSUE_NUM, new RegWriter(coredef.XLEN)))

  val toExec = IO(new Bundle {
    val input = Vec(coredef.ISSUE_NUM, new InstrExt(coredef.ADDR_WIDTH))
    val commit = Input(UInt(log2Ceil(coredef.ISSUE_NUM+1).W))
    val ntag = Input(UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W))
    val output = Output(Vec(coredef.ISSUE_NUM, new ReservedInstr))

    val flush = Input(Bool())
  })

  val NAME_LENGTH = log2Ceil(coredef.INFLIGHT_INSTR_LIMIT)
  val REG_NUM = 32 // TODO: do we need to make this configurable?
  val REG_ADDR_LENGTH = log2Ceil(REG_NUM)

  // Reg -> Name map
  val reg2name = RegInit(VecInit(Seq.fill(REG_NUM)(0.U(NAME_LENGTH.W))))
  val name2reg = RegInit(VecInit(Seq.fill(coredef.INFLIGHT_INSTR_LIMIT)(0.U(NAME_LENGTH.W))))
  val regReady = RegInit(VecInit(Seq.fill(REG_NUM)(true.B)))
  val renamedSetUpcoming = Wire(UInt(coredef.INFLIGHT_INSTR_LIMIT.W))
  renamedSetUpcoming := 0.U

  val nextUp = RegInit(1.U)
  // Reg 0 is always mapped to name 0

  var state = (reg2name, name2reg, regReady, 0.U(coredef.INFLIGHT_INSTR_LIMIT.W), nextUp)
  for(((row, out), idx) <- toExec.input.zip(toExec.output).zipWithIndex) {
    val reg2nameMod = Wire(reg2name.cloneType)
    val name2regMod = Wire(name2reg.cloneType)
    val regReadyMod = Wire(regReady.cloneType)
    val renamedSet = Wire(state._4.cloneType)
    val nextUpMod = Wire(state._5.cloneType)

    // Assign instr and tag
    out.instr := row.instr
    for((orow, idx) <- toExec.output.zipWithIndex) {
      out.tag := toExec.ntag +% idx.U
    }

    // Rename rd
    val rd = row.instr.getRd
    when(row.vacant || rd === 0.U) {
      reg2nameMod := state._1
      name2regMod := state._2
      regReadyMod := state._3
      renamedSet := state._4
      nextUpMod := state._5
  
      out.rdname := 0.U
    }.otherwise {
      reg2nameMod(rd) := state._5
      name2regMod(state._5) := rd
      regReadyMod(rd) := false.B
      renamedSet(rd) := true.B
      nextUpMod := Mux(state._5 === (coredef.INFLIGHT_INSTR_LIMIT-1).U, 1.U, state._5 +% 1.U)

      out.rdname := state._5
    }

    // Read rs
    val rs1 = row.instr.getRs1
    val rs2 = row.instr.getRs2

    rr(idx*2).addr := rs1
    rr(idx*2+1).addr := rs2

    when(rs1 === 0.U) {
      out.rs1name := DontCare
      out.rs1ready := true.B
      out.rs1val := 0.U

      rr(idx*2).data := DontCare
    }.elsewhen(state._3(rs1)) {
      // Ready, reading from regfile
      out.rs1name := DontCare
      out.rs1ready := true.B
      out.rs1val := rr(idx*2).data
    }.otherwise {
      // Not ready yet
      out.rs1name := state._1(rs1)
      out.rs1ready := false.B
      out.rs1val := DontCare
    }

    when(rs2 === 0.U) {
      out.rs2name := DontCare
      out.rs2ready := true.B
      out.rs2val := 0.U

      rr(idx*2+1).data := DontCare
    }.elsewhen(state._3(rs2)) {
      // Ready, reading from regfile
      out.rs2name := DontCare
      out.rs2ready := true.B
      out.rs2val := rr(idx*2+1).data
    }.otherwise {
      // Not ready yet
      out.rs2name := state._1(rs2)
      out.rs2ready := false.B
      out.rs2val := DontCare

      rr(idx*2+1).data := DontCare
    }

    state = (reg2nameMod, name2regMod, regReadyMod, renamedSet, nextUpMod)

    when(toExec.commit === (idx+1).U) {
      reg2name := reg2nameMod
      name2reg := name2regMod
      regReady := regReadyMod
      renamedSetUpcoming := renamedSet
      nextUp := nextUpMod
    }
  }

  // Write-backs
  for(w <- rw) {
    w.addr := 0.U
    w.data := DontCare
  }

  for((ent, w) <- cdb.entries.zip(rw)) {
    // Later ones has higher priority
    when(ent.name =/= 0.U) {
      val reg = name2reg(ent.name)

      when(reg2name(reg) === ent.name && !renamedSetUpcoming(reg)) {
        regReady(ent.name) := true.B
      }

      w.addr := reg
      w.data := ent.data
    }
  }

  for(ent <- cdb.entries) {
    for(out <- toExec.output) {
      when(ent.name =/= 0.U && ent.name === out.rs1name) {
        out.rs1val := ent.data
        out.rs1ready := true.B
      }
    }
  }

  // Lastly, handles flush
  when(toExec.flush) {
    reg2name := VecInit(Seq.fill(REG_NUM)(0.U(NAME_LENGTH.W)))
    name2reg := VecInit(Seq.fill(coredef.INFLIGHT_INSTR_LIMIT)(0.U(NAME_LENGTH.W)))
    regReady := RegInit(VecInit(Seq.fill(REG_NUM)(true.B)))
    nextUp := 1.U
  }
}
