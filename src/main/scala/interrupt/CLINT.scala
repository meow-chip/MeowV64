package interrupt

import chisel3._
import chisel3.experimental._
import chisel3.util._
import multicore.MulticoreDef

object CLINT {
  val CLINT_REGION_START = BigInt("02000000", 16)
  val CLINT_REGION_SIZE = 0x10000
  val CLINT_ADDR_WIDTH = log2Ceil(CLINT_REGION_SIZE)
}

object CLINTMMIODef extends {
  override val ADDR_WIDTH: Int = CLINT.CLINT_ADDR_WIDTH
  override val XLEN: Int = 64
} with MMIODef

object CLINTMapping extends {
  override val MAPPED_START = CLINT.CLINT_REGION_START
  override val MAPPED_SIZE = BigInt(CLINT.CLINT_REGION_SIZE)
} with MMIOMapping

class LocalInt extends Bundle {
  val msip = Bool()
  val mtip = Bool()
}

class CLINT(implicit mcdef: MulticoreDef) extends MultiIOModule {
  val toL2 = IO(new MMIOAccess(CLINTMMIODef))
  val ints = IO(Output(Vec(mcdef.CORE_COUNT, new LocalInt)))
  val time = IO(Output(UInt(64.W)))

  toL2.req.nodeq()
  toL2.resp.bits := DontCare
  toL2.resp.valid := false.B

  // Timer
  val mtime = RegInit(0.U(64.W))
  mtime := mtime +% 1.U

  time := mtime

  val mtimecmp = RegInit(VecInit(Seq.fill(mcdef.CORE_COUNT)(0.U(64.W))))
  for((c, m) <- ints.zip(mtimecmp)) {
    c.mtip := m < mtime
  }

  val msip = RegInit(VecInit(Seq.fill(mcdef.CORE_COUNT)(false.B)))
  for((c, s) <- ints.zip(msip)) {
    c.msip := s
  }

  object State extends ChiselEnum {
    val idle, commit = Value
  }

  object Seg extends ChiselEnum {
    val msip, mtimecmp, mtime = Value
  }

  val state = RegInit(State.idle)
  val seg = Reg(Seg())
  val idx = Reg(UInt(log2Ceil(mcdef.CORE_COUNT).W))
  val wdata = Reg(UInt(64.W))
  val write = RegInit(false.B)

  switch(state) {
    is(State.idle) {
      val cur = toL2.req.deq()
      seg := DontCare
      idx := DontCare
      wdata := cur.wdata
      write := cur.op === MMIOReqOp.write

      when(cur.addr < 0x4000.U) {
        seg := Seg.msip
        idx := cur.addr(11, 0) >> 2
      }.elsewhen(cur.addr =/= 0xBFF8.U) {
        seg := Seg.mtimecmp
        idx := cur.addr(11, 0) >> 3
      }.otherwise {
        seg := Seg.mtime
      }

      when(toL2.req.fire()) {
        state := State.commit
      }
    }

    is(State.commit) {
      state := State.idle

      toL2.resp.bits := DontCare
      toL2.resp.valid := true.B

      switch(seg) {
        is(Seg.msip) {
          toL2.resp.bits := msip(idx)
        }

        is(Seg.mtimecmp) {
          toL2.resp.bits := mtimecmp(idx)
        }

        is(Seg.mtime) {
          toL2.resp.bits := mtime
        }
      }

      when(write) {
        switch(seg) {
          is(Seg.msip) {
            msip(idx) := wdata(0)
          }

          is(Seg.mtimecmp) {
            mtimecmp(idx) := wdata
          }

          is(Seg.mtime) {
            mtime := wdata
          }
        }
      }
    }
  }
}
