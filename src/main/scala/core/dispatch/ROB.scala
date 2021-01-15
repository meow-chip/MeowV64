package core.dispatch

import chisel3._

class ROBEntry extends Bundle {
  val occupied = Bool()
  val done = Bool()
  val ex = Bool()
}