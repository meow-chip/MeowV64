package meowv64.core

import chisel3.util.log2Ceil
import meowv64.cache._
import meowv64.instr.ExecUnitType
import meowv64.reg.RegType

abstract class CoreDef {
  outer =>
  val XLEN: Int = 64
  val VLEN: Int = 256
  val VADDR_WIDTH: Int = 48
  val PADDR_WIDTH: Int = 56
  val FETCH_NUM: Int = 2
  val ISSUE_NUM: Int = 2
  val RETIRE_NUM: Int = 2
  val ISSUE_FIFO_DEPTH: Int = FETCH_NUM * 4
  val INIT_VEC: BigInt = BigInt("FFFF20000000", 16)

  // branch history table
  val BHT_SIZE: Int = 256
  val BHT_WIDTH: Int = 2
  // branch history register
  val BHR_WIDTH: Int = 2
  // write bypass entries in BPU
  val BPU_WRITE_BYPASS_COUNT: Int = 8

  val TLB_SIZE: Int = 32

  val HART_ID: Int

  /** This is one larger than the actual maximum number, because we are
    * reserving name 0 for reg 0
    */
  val INFLIGHT_INSTR_LIMIT = 32

  /** Execution units
    */
  val EXECUTION_UNITS: Seq[Seq[ExecUnitType.Type]] = Seq(
    // port 1: ALU + Branch + CSR + Bypass
    Seq(
      ExecUnitType.alu,
      ExecUnitType.branch,
      ExecUnitType.csr,
      ExecUnitType.bypass
    ),
    // port 2: ALU + Mul + Div
    Seq(
      ExecUnitType.alu,
      ExecUnitType.mul,
      ExecUnitType.div
    ),
    // port 3: FMA + FloatMisc + FDivSqrt
    Seq(
      ExecUnitType.fma,
      ExecUnitType.floatMisc,
      ExecUnitType.fDivSqrt
    ),
    // port 4: LSU
    Seq(ExecUnitType.lsu)
  )

  val UNIT_COUNT: Int = EXECUTION_UNITS.length
  val RESERVATION_STATION_DEPTHS = Seq(
    8,
    4,
    4,
    16
  )

  /** L1 line with in bytes
    */
  val L1_LINE_BYTES: Int = 16

  /** Return address stack size
    */
  val RAS_SIZE: Int = 8;

  /** List of (register type, width)
    */
  def REGISTER_TYPES: Seq[(RegType.Type, Int)] =
    Seq((RegType.integer, XLEN), (RegType.float, XLEN), (RegType.vector, VLEN));

  /** List of supported float types
    */
  def FLOAT_TYPES: Seq[FloatType] = Seq(FloatS, FloatD)

  object L1I
      extends {
        val ADDR_WIDTH: Int = outer.PADDR_WIDTH
        val ASSOC: Int = 2
        val LINE_BYTES: Int = outer.L1_LINE_BYTES
        val SIZE_BYTES: Int = 2048 // 2KB L1 I
        val TRANSFER_WIDTH: Int = 64 // 64 bits
        val XLEN: Int = outer.XLEN
      }
      with L1Opts

  object L1D
      extends {
        val ADDR_WIDTH: Int = outer.PADDR_WIDTH
        val ASSOC: Int = 2
        val LINE_BYTES: Int = outer.L1_LINE_BYTES
        val SIZE_BYTES: Int = 2048 // 2KB L1 D
        val TRANSFER_WIDTH: Int = outer.XLEN // Currently, this is required
        val XLEN: Int = outer.XLEN

        val WRITE_BUF_DEPTH: Int = 4
      }
      with L1DOpts

  def tlbIdxWidth = log2Ceil(TLB_SIZE)
  def vpnWidth = VADDR_WIDTH - 12
  def ppnWidth = PADDR_WIDTH - 12
}

// TODO: moves into MulticoreDef
object CoreDef {
  def default(id: Int, initVec: BigInt) = {
    new CoreDef {
      override val HART_ID = id
      override val INIT_VEC = initVec
    }
  }
}
