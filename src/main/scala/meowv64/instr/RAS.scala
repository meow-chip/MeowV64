package meowv64.instr

import chisel3._
import meowv64.core.CoreDef
import chisel3.util._

class RAS(implicit val coredef: CoreDef) extends MultiIOModule {
  val toIF = IO(new Bundle {
    val push = Flipped(ValidIO(UInt(coredef.XLEN.W)))
    val pop = DecoupledIO(UInt(coredef.XLEN.W))
  })

  val toExec = IO(new Bundle {
    val realign = Flipped(ValidIO(new Bundle {
      val ptr = UInt(log2Ceil(coredef.RAS_SIZE).W)
      val empty = Bool()
    }))
  })

  val store = Reg(Vec(coredef.RAS_SIZE, UInt(coredef.XLEN.W)))
  val ptr = RegInit(0.U(log2Ceil(coredef.RAS_SIZE).W))
  val empty = RegInit(true.B)

  toIF.pop.bits := store(ptr -% 1.U)
  toIF.pop.valid := !empty

  when(!toExec.realign.valid) {
    when(toIF.push.valid) {
      val nptr = ptr +% 1.U

      when(nptr =/= 0.U || !empty || toIF.pop.fire()) {
        when(!toIF.pop.fire()) {
          ptr := nptr
        }
        store(ptr) := toIF.push.bits
        empty := false.B
      }.otherwise { // is full and pushing
        assert(ptr === (coredef.RAS_SIZE-1).U)
        for(i <- (0 until coredef.RAS_SIZE)) {
          if(i == coredef.RAS_SIZE-1) {
            store(i) := toIF.push.bits
          } else {
            store(i) := store(i+1)
          }
        }
      }
    }.elsewhen(toIF.pop.fire()) {
      ptr := ptr -% 1.U
    }
  }.otherwise {
    ptr := toExec.realign.bits.ptr
    empty := toExec.realign.bits.empty
  }
}
