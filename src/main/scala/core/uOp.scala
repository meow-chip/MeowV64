package core

import chisel3.experimental._
import chisel3._

object uOpType extends ChiselEnum {
  /**
    * Move elimination
    */
  val Move = Value

  /**
   * Direct exceptions, e.g. Fetch page-fault, Inval instr, ECALL / etc
   */
  val Ex = Value

  /**
   * SYSTEM (CSR, xRET)
   */
  val System = Value

  /**
   * Integer instructions, excluding memory
   */
  val iOp = Value
  val iOp32 = Value
  val iOpImm = Value
  val iOpImm32 = Value
  val iLui = Value
  val iAuipc = Value

  /**
   * Memory instructions
   */
  val mLoad = Value
  // Potentially supporting spliting sData / sAddr?
  val mStore = Value
  // val mStoreData = Value
  // val mStoreAddr = Value
  val mFence = Value
  val mAmo = Value

  /**
   * Floating point instructions
   * TODO: impl me
   */
  // val fAdd = Value
}

// RV32 / RV64 has 32 standard gp-reg
class LRegNum extends Bundle {
  val num = UInt(5.W)
}

class uOp extends Bundle {
  val t = uOpType()

  // TODO: rs3 + uOp fusion?
  val rs1 = new LRegNum()
  val rs2 = new LRegNum()
  val rd = new LRegNum()

  val imm = UInt(32.W)
  val ex = UInt(4.W) // Early exception

  val funct3 = UInt(7.W)
  val funct7 = UInt(7.W)
}