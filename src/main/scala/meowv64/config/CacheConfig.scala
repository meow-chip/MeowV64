package meowv64.config

import spinal.core._
import meowv64.mem._
import meowv64._

case class CacheConfig(
  val assoc_size: Int,
  val assoc_cnt: Int,
  val line_size: Int,
  val bank_cnt: Int = 1,
) {
  require(isPow2(assoc_size))
  require(isPow2(line_size))
  def line_per_assoc: Int = assoc_size / (line_size)
  def offset_width: Int = log2Up(line_size)
  def bankidx_width = log2Up(bank_cnt)
  def index_width: Int = log2Up(line_per_assoc)
  def tag_width(alen: Int) = alen - offset_width - bankidx_width - index_width

  def offset(addr: UInt): UInt = addr(0, offset_width bits)
  def bankidx(addr: UInt): UInt = addr(offset_width, bankidx_width bits)
  def index(addr: UInt): UInt = addr(offset_width + bankidx_width, index_width bits)
  def tag(addr: UInt): UInt = addr >> (index_width + offset_width + bankidx_width)
  def line_width = line_size * 8
}