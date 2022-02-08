package meowv64.config

import chisel3._
import chisel3.util._
import meowv64.mem._
import meowv64._

case class CacheConfig(
  val assoc_size: Int,
  val assoc_cnt: Int,
  val line_size: Int,
  val inst_cnt: Int = 1,
) {
  require(isPow2(assoc_size))
  require(isPow2(line_size))
  def line_per_assoc: Int = assoc_size / (line_size)
  def offset_width: Int = log2Ceil(line_size)
  def instidx_width = log2Ceil(inst_cnt)
  def index_width: Int = log2Ceil(line_per_assoc)
  def tag_width(alen: Int) = alen - offset_width - instidx_width - index_width

  def offset(addr: UInt): UInt = addr(offset_width - 1, 0)
  def instidx(addr: UInt): UInt = (addr >> offset_width)(instidx_width - 1, 0)
  def index(addr: UInt): UInt = (addr >> (offset_width + instidx_width))(index_width - 1, 0)
  def tag(addr: UInt): UInt = addr >> (index_width + offset_width + instidx_width)
  def line_width = line_size * 8
}