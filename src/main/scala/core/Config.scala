package core

import chisel3.util._

abstract class ICacheConfig {
  val assoc = 2
  val rowInstr = 16
  val totByte = 4096

  def rowPerAssoc = totByte / (rowInstr * 2) / assoc
  def offsetWidth = log2Ceil(rowInstr * 2)
  def idxWidth = log2Ceil(rowPerAssoc)
  def tagWidth(implicit config: Config) = config.pAddrWidth - offsetWidth - idxWidth
}

object DefaultICacheConfig extends ICacheConfig

abstract class Config {
  val xlen = 64
  val pAddrWidth = 56

  val iCache = DefaultICacheConfig
  val fetchWidth = 2
  val initVec = BigInt("0x80000000")

  def fetchOffset = log2Ceil(fetchWidth) + 1
}

object DefaultConfig extends Config