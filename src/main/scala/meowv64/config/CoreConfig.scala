package meowv64.config

import spinal.core._
import meowv64._

// In bytes
case class CacheConfig(
  val assoc_size: Int,
  val assoc_cnt: Int,
  val line_width: Int,
) {
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

case class CoreConfig(
  val xlen: Int,
  val init_vec: BigInt,

  val fetch_width: Int,
  val decode_width: Int,

  val ic: CacheConfig,
) {
  def rint: UInt = UInt(xlen bits)
  def frontend_membus_params = MemBusParams(
    addr_width = xlen,
    data_width = xlen,
    id_width = 4,
  )
}

object DefaultCoreConfig extends CoreConfig(
  xlen = 64,
  init_vec = BigInt("0x80000000"),
  fetch_width = 8,
  decode_width = 4,

  ic = CacheConfig(
    assoc_size = 4096,
    assoc_cnt = 2,
    line_width = 64 * 8,
  )
)