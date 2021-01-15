package core

abstract class Config {
  val xlen = 64
  val pAddrWidth = 56

  val fetchWidth = 2
}

object DefaultConfig extends Config