package meowv64.config

import spinal.core._
import meowv64.mem._
import meowv64._

case class CoreConfig(
  val xlen: Int,
  val init_vec: BigInt,

  val fetch_width: Int,
  val decode_width: Int,

  val ic: CacheConfig,
) {
  def rint: UInt = UInt(xlen bits)
  def membus_params(bus_type: MemBusType) = MemBusParams(
    bus_type,
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
    line_size = 64,
  )
)