package meowv64.mem

import chisel3._
import Chisel.log2Up

case class Axi4Config(
  val id_width: Int,
  val addr_width: Int,
  val data_width: Int,
)

class Axi4(cfg: Axi4Config) extends Bundle {
  val awid = Output(UInt(cfg.id_width.W))
  val awvalid = Output(Bool())
  val awready = Input(Bool())
  val awaddr = Output(UInt(cfg.addr_width.W))
  val awlen = Output(UInt(8.W))
  val awburst = Output(UInt(2.W))
  val awsize = Output(UInt(log2Up(cfg.data_width / 8).W))

  val wvalid = Output(Bool())
  val wready = Input(Bool())
  val wdata = Output(UInt(cfg.data_width.W))
  val wstrb = Output(UInt((cfg.data_width / 8).W))
  val wlast = Output(Bool())

  val bid = Output(UInt(cfg.id_width.W))
  val bvalid = Input(Bool())
  val bready = Output(Bool())
  val bresp = Input(UInt(2.W))

  val arid = Output(UInt(cfg.id_width.W))
  val arvalid = Output(Bool())
  val arready = Input(Bool())
  val araddr = Output(UInt(cfg.addr_width.W))
  val arsize = Output(UInt(log2Up(cfg.data_width / 8).W))
  val arlen = Output(UInt(8.W))
  val arburst = Output(UInt(2.W))

  val rid = Output(UInt(cfg.id_width.W))
  val rvalid = Input(Bool())
  val rready = Output(Bool())
  val rdata = Input(UInt(cfg.data_width.W))
  val rresp = Input(UInt(2.W))
  val rlast = Input(Bool())
}