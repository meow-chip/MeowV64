package meowv64.config

import meowv64.mem._

case class L2Config(
  val base: CacheConfig,
  val max_pending_inv: Int = 4,
)

case class MulticoreConfig(
  val cores: Seq[CoreConfig],
  // TODO: assert cores can generate uniform intc (has same fetch width, xlen, ...)

  val l2: L2Config,
  val eint_cnt: Int,
) {
  def membus_params = MemBusParams(
    bus_type = L2,
    addr_width = 64,
    data_width = 64,
    id_width = 4,
  )
}


object DefaultMulticoreConfig extends MulticoreConfig(
  cores = Seq(DefaultCoreConfig),
  l2 = L2Config(
    base = CacheConfig(
      line_size = 64,
      assoc_size = 64 * 1024,
      assoc_cnt = 4,
    )
  ),
  eint_cnt = 4,
)