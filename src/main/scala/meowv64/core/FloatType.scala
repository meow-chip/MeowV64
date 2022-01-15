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

  // auto implemented
  // total bits
  def width(): Int = exp() + sig()
  // HF width in bits
  def widthHardfloat(): Int = width() + 1

  // conversion to hardfloat internal representation
  def toHardfloat(n: UInt) = recFNFromFN(exp(), sig(), n)
  def fromHardfloat(n: UInt) = fNFromRecFN(exp(), sig(), n)

  // generate the representation of 1.0
  // chisel
  def one() =
    (((BigInt(1) << (exp() - 1)) - 1) << (sig() - 1)).U(width().W)
  def oneHardfloat() =
    (BigInt(1) << (exp() + sig() - 1)).U(widthHardfloat().W)

  // NaN boxing/unboxing
  def box(n: UInt, xlen: Int) =
    ((BigInt(1) << xlen) - (BigInt(1) << width)).U | n
  def unbox(n: UInt) = n(width - 1, 0)
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
}

/** 32-bit Float
  */
object FloatS extends FloatType {
  def exp() = 8
  def sig() = 24
  def kind() = FpKind.S
}

/** 16-bit Half Float
  */
object FloatH extends FloatType {
  def exp() = 5
  def sig() = 11
  def kind() = FpKind.H
}
