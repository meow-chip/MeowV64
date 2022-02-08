package meowv64.mem

import chisel3._
import chisel3.util._
import chisel3.experimental._

sealed class MemBusType(
  // Can write
  val with_write: Boolean,

  // Coherence master
  val with_coherence: Boolean,

  // LRSC/AMO
  val with_subop: Boolean,

  // Direct memory port, no burst, has size
  val with_direct: Boolean,
);

case object Frontend extends MemBusType(false, false, false, false)
case object Backend extends MemBusType(true, true, true, false)
case object Uncached extends MemBusType(true, false, false, true)
case object L2 extends MemBusType(true, false, false, false)
case object External extends MemBusType(true, false, false, true)

case class MemBusParams(
  val bus_type: MemBusType,
  val addr_width: Int,
  val data_width: Int,
  val id_width: Int,
) {
  val to_axi_config = Axi4Config(
    id_width = id_width,
    addr_width = addr_width,
    data_width = data_width,
  )
}

/**
  * Internal memory bus. This is heavely inspired by the BMB(Banana memory bus).
  * 
  * To avoid deadlock, all master and slave should handles the channels in the following priority:
  * - ack: Essentially TL-E
  * - resp: Essentially TL-C
  * - uplink: Essentially TL-D
  * - inv: Essentially TL-B
  * - downlink: Data part of TL-A
  * - cmd: Cmd part of TL-A
  *
  * @param isInstr: If this memory bus's master is an I$
  */
class MemBus(val params: MemBusParams) extends Bundle {
  val cmd = DecoupledIO(new MemBusCmd(params))
  val uplink = Flipped(DecoupledIO(new MemBusUplink(params)))
  val downlink = if(params.bus_type.with_write) DecoupledIO(new MemBusDownlink(params)) else null
  val inv = if(params.bus_type.with_coherence) Flipped(DecoupledIO(new MemBusInv(params))) else null
  val resp = if(params.bus_type.with_coherence) DecoupledIO(new MemBusInvResp(params)) else null
  val ack = if(params.bus_type.with_coherence) DecoupledIO(new MemBusOccupyAck(params)) else null

  def toAxi4: Axi4 = {
    require(!params.bus_type.with_subop)
    require(!params.bus_type.with_coherence)
    require(params.bus_type.with_write)
    require(params.bus_type.with_direct)
    val axi4 = new Axi4(params.to_axi_config)

    // FIXME: Impl
    ???
  }
}

object MemBusOp extends ChiselEnum {
  val read, write, occupy, amo = Value
}

object MemBusSubOpIdx {
  // Occupy may contains no data
  val WITH_DATA = 0

  // Occupy / read maybe LR/SC
  val LR_SC = 1

  // Write may release ownership
  val RELEASE = 2
}

// Ids are shared between read and writes
class MemBusCmd(val params: MemBusParams) extends Bundle {
  val id = UInt(params.id_width.W)

  val op = if(params.bus_type.with_write) MemBusOp() else null
  val subop = if(params.bus_type.with_subop) Bits(5.W) else null

  val size = if(params.bus_type.with_direct) UInt(2.W) else null // Supports up to 2^3 = 64
  val burst = if(params.bus_type.with_direct) UInt(8.W) else null // Supports up to 256
  // No write enable for direct writes because we don't need them!

  // Keyword first sematic, beat count is always determined by cache line width
  val addr = UInt(params.addr_width.W)
}

// Master -> Slave
class MemBusDownlink(val params: MemBusParams) extends Bundle {
  // No write interleaving is allowed.
  val data = Bits(params.data_width.W)
}

// Slave -> Master
class MemBusUplink(val params: MemBusParams) extends Bundle {
  val id = UInt(params.id_width.W)
  val data = Bits(params.data_width.W)
}

class MemBusInvOp extends ChiselEnum {
  val flush, inv = Value
}

class MemBusInv(val params: MemBusParams) extends Bundle {
  val op = new MemBusInvOp
  val addr = UInt(params.addr_width.W)
}

class MemBusInvResp(val params: MemBusParams) extends Bundle {
  val with_data = Bool()
  val data = Bits(params.data_width.W)
}

class MemBusOccupyAck(val params: MemBusParams) extends Bundle {
  val id = Bits(params.id_width.W)
}