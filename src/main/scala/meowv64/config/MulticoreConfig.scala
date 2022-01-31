package meowv64.config

case class MulticoreConfig(
  val cores: Seq[CoreConfig],
  // TODO: assert cores can generate uniform intc (has same fetch width, xlen, ...)
  // TODO: l2 config

  val eint_cnt: Int,
)

object DefaultMulticoreConfig extends MulticoreConfig(
  cores = Seq(DefaultCoreConfig),
  eint_cnt = 4,
)