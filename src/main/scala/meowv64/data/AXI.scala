package meowv64.data

import chisel3._

// AXI master port, without ACLK / ARSESTn
class AXI(
    val DATA_WIDTH: Int,
    val ADDR_WIDTH: Int = 48,
    val ID_WIDTH: Int = 4
) extends Bundle {
  if (DATA_WIDTH == 32) throw new Error()
  // TODO: asserts DATA_WIDTH % 8 === 0
  val AWID = Output(UInt(ID_WIDTH.W))
  val AWADDR = Output(UInt(ADDR_WIDTH.W))
  val AWLEN = Output(UInt(8.W))
  val AWSIZE = Output(UInt(3.W))
  val AWBURST = Output(UInt(2.W))
  // AXI4 removes AWLOCK
  val AWCACHE = Output(UInt(4.W))
  val AWPROT = Output(UInt(3.W))
  val AWQOS = Output(UInt(3.W))
  val AWREGION = Output(UInt(4.W))
  // We ignore user signals
  val AWVALID = Output(Bool())
  val AWREADY = Input(Bool())

  // AXI4 removes WID
  val WDATA = Output(UInt(DATA_WIDTH.W))
  val WSTRB = Output(UInt((DATA_WIDTH / 8).W))
  val WLAST = Output(Bool())
  val WVALID = Output(Bool())
  val WREADY = Input(Bool())

  val BID = Input(UInt(ID_WIDTH.W))
  val BRESP = Input(UInt(2.W))
  val BVALID = Input(Bool())
  val BREADY = Output(Bool())

  val ARID = Output(UInt(ID_WIDTH.W))
  val ARADDR = Output(UInt(ADDR_WIDTH.W))
  val ARLEN = Output(UInt(8.W))
  val ARSIZE = Output(UInt(3.W))
  val ARBURST = Output(UInt(2.W))
  val ARCACHE = Output(UInt(4.W))
  val ARPROT = Output(UInt(3.W))
  val ARQOS = Output(UInt(3.W))
  val ARREGION = Output(UInt(4.W))
  val ARVALID = Output(Bool())
  val ARREADY = Input(Bool())

  val RID = Input(UInt(ID_WIDTH.W))
  val RDATA = Input(UInt(DATA_WIDTH.W))
  val RRESP = Input(UInt(2.W))
  val RLAST = Input(Bool())
  val RVALID = Input(Bool())
  val RREADY = Output(Bool())
}

object AXI {
  object Constants {
    object Resp {
      val OKAY = 0
      val EXOKAY = 1
      val SLVERR = 2
      val DECERR = 3
    }

    object Size {
      val S1 = 0
      val S2 = 1
      val S4 = 2
      val S8 = 3
      val S16 = 4
      val S32 = 5
      val S64 = 6
      val S128 = 7

      def from(width: Int): Int = width match {
        case 1   => S1
        case 2   => S2
        case 4   => S4
        case 8   => S8
        case 16  => S16
        case 32  => S32
        case 64  => S64
        case 128 => S128
      }
    }

    object Burst {
      val FIXED = 0
      val INCR = 1
      val WRAP = 2
    }
  }
}
