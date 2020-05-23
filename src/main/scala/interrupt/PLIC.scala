package interrupt

import chisel3._
import chisel3.util._
import chisel3.experimental._

abstract class PLICDef {
  val CONTEXT_COUNT: Int
  val MAX_PRIORITY: Int
  val MAX_SOURCE: Int
}

object PLIC {
  val PLIC_REGION_START = BigInt("0C000000", 16)
  val PLIC_REGION_SIZE = 0x3000000
  val PLIC_ADDR_WIDTH = log2Ceil(PLIC_REGION_SIZE)
}

object PLICMMIODef extends {
  override val ADDR_WIDTH: Int = PLIC.PLIC_ADDR_WIDTH
  override val XLEN: Int = 64
} with MMIODef

object PLICMapping extends {
  override val MAPPED_START = PLIC.PLIC_REGION_START
  override val MAPPED_SIZE = BigInt(PLIC.PLIC_REGION_SIZE)
} with MMIOMapping

object PLICAddrSpace extends ChiselEnum {
  val priority, pending, enable, context = Value

  def fromOffset(offset: UInt): PLICAddrSpace.Type = {
    val ret = Wire(PLICAddrSpace());
    when(offset < 0x1000.U) {
      ret := priority
    }.elsewhen(offset < 0x2000.U) {
      ret := pending
    }.elsewhen(offset < 0x200000.U) {
      ret := enable
    }.otherwise {
      ret := context
    }

    ret
  }
}

class PLIC(val pdef: PLICDef) extends MultiIOModule {
  val source = IO(Input(UInt((pdef.MAX_SOURCE+1).W))) // Interrupt source, don't use 0
  val eints = IO(Output(Vec(pdef.CONTEXT_COUNT, Bool())))
  val toL2 = IO(new MMIOAccess(PLICMMIODef))

  val priorities = RegInit(VecInit(
    Seq.fill(pdef.MAX_SOURCE + 1)(
      0.U(log2Ceil(pdef.MAX_PRIORITY+1).W)
    )
  ))
  val thresholds = RegInit(VecInit(
    Seq.fill(pdef.CONTEXT_COUNT)(
      0.U(log2Ceil(pdef.MAX_PRIORITY+1).W)
    )
  ))
  val enables = RegInit(VecInit(
    Seq.fill(pdef.CONTEXT_COUNT)(
      VecInit(Seq.fill(Math.ceil((pdef.MAX_SOURCE + 1) / 32f).toInt)(0.U(32.W)))
    )
  ))
  val claimed = RegInit(VecInit(
    Seq.fill(pdef.CONTEXT_COUNT)(
      VecInit(Seq.fill(pdef.MAX_SOURCE + 1)(false.B))
    )
  ))

  val masked = WireDefault(priorities)
  masked(0) := 0.U
  val castedEnables = Wire(Vec(pdef.CONTEXT_COUNT, UInt((pdef.MAX_PRIORITY+1).W)))
  for((casted, enable) <- castedEnables.zip(enables)) {
    casted := enable.asUInt()
  }
  val castedSource = Wire(Vec(Math.ceil((pdef.MAX_SOURCE + 1) / 32f).toInt, UInt(32.W)))
  castedSource := source.asTypeOf(castedSource)

  // Priority > Threshold
  val qualifieds = thresholds.map(
    (threshold) => VecInit(masked.map(priority => priority > threshold)).asUInt()
  )

  // Qualified and enabled
  val effectives = qualifieds.zip(castedEnables).map({ case (qualified, enable) => qualified & enable })

  // Gated inputs
  val gated = claimed.foldLeft(source)({
    case (acc, claimed) => acc & ~(claimed.asUInt())
  })

  // Gated & effective
  val outputs = effectives.map((effective) => effective & gated)
  eints := outputs.map(output => output.orR())

  // Most prioritized interrupt
  val claiming = VecInit(outputs.map((output) => PriorityEncoderOH(output)))

  // MMIO interface
  val req = toL2.req.deq()
  toL2.resp.valid := toL2.req.fire()
  toL2.resp.bits := DontCare
  val offset = (req.addr -% PLIC.PLIC_REGION_START.U)(PLIC.PLIC_ADDR_WIDTH-1, 0)
  switch(PLICAddrSpace.fromOffset(offset)) {
    is(PLICAddrSpace.priority) {
      toL2.resp.bits := priorities(offset >> 2)

      when(toL2.req.fire() && req.op === MMIOReqOp.write) {
        priorities(offset >> 2) := req.wdata
      }
    }

    is(PLICAddrSpace.pending) {
      toL2.resp.bits := castedSource(offset >> 2)
    }

    is(PLICAddrSpace.enable) {
      val inner = offset - 0x2000.U
      val ctx = inner >> (1024/8)
      toL2.resp.bits := enables(ctx)(inner >> 2)

      when(toL2.req.fire() && req.op === MMIOReqOp.write) {
        enables(ctx)(inner >> 2) := req.wdata
      }
    }

    is(PLICAddrSpace.context) {
      val ctx = (offset - 0x200000.U) >> 12
      toL2.resp.bits := DontCare

      switch(offset(11, 2)) {
        is(0.U) { // Priority threshold
          toL2.resp.bits := thresholds(ctx)

          when(toL2.req.fire() && req.op === MMIOReqOp.write) {
            thresholds(ctx) := req.wdata
          }
        }

        is(1.U) { // Claim/complete
          toL2.resp.bits := claiming(ctx)
          when(toL2.req.fire()) {
            when(req.op === MMIOReqOp.read) {
              claimed(ctx)(claiming(ctx)) := true.B
            }.otherwise {
              claimed(ctx)(req.wdata) := false.B
            }
          }
        }
      }
    }
  }
}
