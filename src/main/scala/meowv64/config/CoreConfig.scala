package meowv64.config

import spinal.core._

// In bytes
trait CacheConfig {
  val assoc_size: Int
  val assoc_cnt: Int
  val line_width: Int


  require(isPow2(assoc_size))
  require(isPow2(line_width))
  def line_per_assoc: Int = assoc_size / line_width
  def offset_width: Int = log2Up(line_width)
  def index_width: Int = log2Up(line_per_assoc)
  def tag_width(alen: Int) = alen - offset_width - index_width

  def offset(addr: UInt): UInt = addr(0, offset_width bits)
  def index(addr: UInt): UInt = addr(offset_width, index_width bits)
  def tag(addr: UInt): UInt = addr >> (index_width + offset_width)
}

trait CoreConfig {
  val xlen: Int
  val init_vec: BigInt

  val fetch_width: Int
  val decode_width: Int

  val ic: CacheConfig

  def rint: UInt = UInt(xlen bits)
}

object DefaultCoreConfig extends CoreConfig {
  val xlen = 64
  val init_vec = BigInt("0x80000000")

  val fetch_width: Int = 8
  val decode_width: Int = 4

  val ic = new CacheConfig {
    val assoc_size: Int = 4096
    val assoc_cnt: Int = 2
    val line_width: Int = 64 // B
  }
}