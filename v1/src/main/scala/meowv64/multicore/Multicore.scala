package meowv64.multicore

import chisel3._
import meowv64.cache.L2Cache
import meowv64.core.Core
import meowv64.core.CoreDebug
import meowv64.core.CoreDef
import meowv64.data._
import meowv64.interrupt.CLINT
import meowv64.interrupt.PLIC

class Multicore(implicit val mcdef: MulticoreDef = DefaultDef) extends Module {
  val io = IO(new Bundle {
    val axi = new AXI(mcdef.XLEN, mcdef.PADDR_WIDTH)
    val eints = Input(Vec(mcdef.INTERRUPT_CNT + 1, Bool()))

    // Debug infos
    val debug = Output(
      Vec(mcdef.CORE_COUNT, new CoreDebug()(CoreDef.default(0, mcdef.INIT_VEC)))
    ) // HART_ID doesn't matter here
  })

  val cores = (0 until mcdef.CORE_COUNT).map(id =>
    Module(new Core()(CoreDef.default(id, mcdef.INIT_VEC)))
  )

  val l2 = Module(new L2Cache(mcdef.L2))
  val clint = Module(new CLINT)
  val plic = Module(new PLIC(mcdef.PLIC))

  l2.axi <> io.axi
  l2.mmio(0) <> clint.toL2
  l2.mmio(1) <> plic.toL2
  for (idx <- (0 until mcdef.CORE_COUNT)) {
    l2.ic(idx) <> cores(idx).io.frontend.ic
    l2.dc(idx) <> cores(idx).io.frontend.dc
    l2.directs(idx) <> cores(idx).io.frontend.uc
  }

  for (idx <- (0 until mcdef.CORE_COUNT)) {
    cores(idx).io.int.msip := clint.ints(idx).msip
    cores(idx).io.int.mtip := clint.ints(idx).mtip
    cores(idx).io.time := clint.time
  }

  plic.source := io.eints.asUInt()
  for (idx <- (0 until mcdef.CORE_COUNT)) {
    cores(idx).io.int.meip := plic.eints(idx * 2)
    cores(idx).io.int.seip := plic.eints(idx * 2 + 1)
  }

  io.debug := cores.map(_.io.debug)
}
