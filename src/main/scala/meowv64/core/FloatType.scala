package meowv64.core

import chisel3._
import chisel3.util._
import chisel3.experimental._
import hardfloat.recFNFromFN
import hardfloat.fNFromRecFN

/** Trait for floating point type
  */
trait FloatType {
  // must implement
  // exp bits
  def exp(): Int
  // (total - exp) bits
  def sig(): Int
  def kind(): FpKind.Type
  // riscv instruction fmt field
  def fmt(): UInt

  // auto implemented
  // total bits
  def width(): Int = exp() + sig()
  // HF width in bits
  def widthHardfloat(): Int = width() + 1

  // conversion to hardfloat internal representation
  def toHardfloat(n: UInt) = recFNFromFN(exp(), sig(), n)
  def fromHardfloat(n: UInt) = fNFromRecFN(exp(), sig(), n)

  // generate the representation of 1.0
  def one() =
    (((BigInt(1) << (exp() - 1)) - 1) << (sig() - 1)).U(width().W)
  def oneHardfloat() =
    (BigInt(1) << (exp() + sig() - 1)).U(widthHardfloat().W)

  // generate canonical(quiet) nan
  // 0 1..1 10..0
  def nan() =
    (((BigInt(1) << (exp() + 1)) - 1) << (sig() - 2)).U(width().W)

  // NaN boxing/unboxing
  def box(n: UInt, xlen: Int) =
    ((BigInt(1) << xlen) - (BigInt(1) << width)).U | n
  def unbox(n: UInt, xlen: Int) = {
    if (width == xlen) {
      WireInit(n)
    } else {
      // return canonical nan
      val res = WireInit(nan)
      when(n(xlen - 1, width).andR) {
        // correctly nan boxed
        res := n(width - 1, 0)
      }
      res
    }
  }

  // classify
  def getExp(n: UInt) = n(exp + sig - 2, sig - 1)
  def getSig(n: UInt) = n(sig - 2, 0)
  def isNaN(n: UInt) = getExp(n).andR && getSig(n).orR
  def isInf(n: UInt) = getExp(n).andR && getSig(n) === 0.U
  def isZero(n: UInt) = getExp(n) === 0.U && getSig(n) === 0.U
}

/** Enum of floating point types
  */
object FpKind extends ChiselEnum {
  // Double, Single, Half precision
  val D, S, H = Value

  implicit def bitpat(op: FpKind.Type): BitPat =
    BitPat(op.litValue.U(getWidth.W))
}

/** 64-bit Double
  */
object FloatD extends FloatType {
  def exp() = 11
  def sig() = 53
  def kind() = FpKind.D
  def fmt() = 1.U
}

/** 32-bit Float
  */
object FloatS extends FloatType {
  def exp() = 8
  def sig() = 24
  def kind() = FpKind.S
  def fmt() = 0.U
}

/** 16-bit Half Float
  */
object FloatH extends FloatType {
  def exp() = 5
  def sig() = 11
  def kind() = FpKind.H
  def fmt() = ??? // not defined
}
