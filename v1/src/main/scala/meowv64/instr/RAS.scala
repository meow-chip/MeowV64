package meowv64.instr

import chisel3._
import chisel3.util._
import meowv64.core.CoreDef

class RAS(implicit val coredef: CoreDef) extends Module {
  val toIF = IO(new Bundle {
    val push = Flipped(Valid(UInt(coredef.XLEN.W)))
    val pop = Decoupled(UInt(coredef.XLEN.W))
  })

  val toExec = IO(new Bundle {
    val realign = Flipped(Valid(new Bundle {
      val ptr = UInt(log2Ceil(coredef.RAS_SIZE).W)
      val empty = Bool()
    }))
  })

  val store = RegInit(VecInit(Seq.fill(coredef.RAS_SIZE)(0.U(coredef.XLEN.W))))
  val ptr = RegInit(0.U(log2Ceil(coredef.RAS_SIZE).W))
  val empty = RegInit(true.B)

  toIF.pop.bits := store(ptr -% 1.U)
  toIF.pop.valid := !empty

  when(!toExec.realign.valid) {
    when(toIF.push.valid) {
      val nptr = ptr +% 1.U

      when(nptr =/= 0.U || !empty || toIF.pop.fire) {
        when(!toIF.pop.fire) {
          ptr := nptr
        }
        store(ptr) := toIF.push.bits
        empty := false.B
      }.otherwise { // is full and pushing
        assert(ptr === (coredef.RAS_SIZE - 1).U)
        for (i <- (0 until coredef.RAS_SIZE)) {
          if (i == coredef.RAS_SIZE - 1) {
            store(i) := toIF.push.bits
          } else {
            store(i) := store(i + 1)
          }
        }
      }
    }.elsewhen(toIF.pop.fire) {
      val nptr = ptr -% 1.U
      ptr := nptr
      when(nptr === 0.U) {
        empty := true.B
      }
    }
  }.otherwise {
    ptr := toExec.realign.bits.ptr
    empty := toExec.realign.bits.empty
  }
}
