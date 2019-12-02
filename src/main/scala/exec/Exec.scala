package exec
import chisel3._
import reg._
import data._
import instr._
import chisel3.util._
import _root_.core.StageCtrl
import instr.Decoder.InstrType
import _root_.core.CSRWriter
import _root_.core.CoreDef
import cache.DCReader
import cache.DCWriter

/**
 * Out-of-order exection (Tomasulo's algorithm)
 * 
 * First we check if instructions are eligible to be issues. Criterias include:
 * - Target reservation station has free slots
 * - Number of in-flight instructions haven't exceeded the limit.
 *   This limit affects our rob buffer length, as well as renamed reg tags' length
 * - Issue FIFO is not depleted
 */
class Exec(implicit val coredef: CoreDef) extends MultiIOModule {
  val io = IO(new Bundle {
    val regReaders = Vec(2, new RegReader)
    val regWriter = new RegWriter

    val ctrl = StageCtrl.stage()

    val branch = Output(new BranchResult)
    val brSrc = Output(UInt(coredef.ADDR_WIDTH.W))

    val csrWriter = new CSRWriter(coredef.XLEN)
  })

  val toIF = IO(new InstrFifoReader(coredef))

  val toDC = IO(new Bundle {
    val r = new DCReader(coredef.L1D)
    val w = new DCWriter(coredef.L1D)
  })
}
