package meowv64.mem

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.Axi4
import spinal.lib.bus.amba4.axi.Axi4Config

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
);

/**
  * Internal memory bus. This is heavely inspired by the BMB(Banana memory bus).
  *
  * @param isInstr: If this memory bus's master is an I$
  */
class MemBus(val params: MemBusParams) extends Bundle with IMasterSlave {
  val cmd = Stream(new MemBusCmd(params))
  val uplink = Stream(new MemBusUplink(params))
  val downlink = if(params.bus_type.with_write) Stream(new MemBusDownlink(params)) else null
  val inv = if(params.bus_type.with_coherence) Stream(new MemBusInv(params)) else null
  val ack = if(params.bus_type.with_coherence) new MemBusInvAck(params) else null

  override def asMaster(): Unit = {
    master(cmd, downlink)
    slave(uplink, inv)
    out(ack)
  }

  object ToAxi4Config extends Axi4Config(
    addressWidth = params.addr_width,
    dataWidth = params.data_width, // TODO: make this configurable
    idWidth = params.id_width,
    useRegion = false,
    useLock = false,
    useCache = false,
    useQos = false,
    useProt = false,
  )

  def toAxi4: Axi4 = {
    require(!params.bus_type.with_subop)
    require(!params.bus_type.with_coherence)
    require(params.bus_type.with_write)
    require(params.bus_type.with_direct)
    val axi4 = new Axi4(ToAxi4Config)

    // FIXME: Impl
    ???
  }
}

class MemBusOp extends SpinalEnum {
  val read, write, occupy, amo = newElement()
}

// TODO figure out encoding
class MemBusSubOp extends SpinalEnum {
  val default = newElement()

  // For r, o
  val lrsc = newElement()
}

// Ids are shared between read and writes
class MemBusCmd(val params: MemBusParams) extends Bundle {
  val id = Bits(params.id_width bits)

  val op = if(params.bus_type.with_write) new MemBusOp else null
  val subop = if(params.bus_type.with_subop) Bits(5 bits) else null

  val size = if(params.bus_type.with_direct) UInt(3 bits) // Supports up to 2^7

  // Keyword first sematic, beat count is always determined by cache line width
  val addr = UInt(params.addr_width bits)
}

// Master -> Slave
class MemBusDownlink(val params: MemBusParams) extends Bundle {
  // No write interleaving is allowed. If the master is current writing,
  // and an invalidation ack with data is present, slave should block the invalidation ack
  val data = Bits(params.data_width bits)
}

// Slave -> Master
class MemBusUplink(val params: MemBusParams) extends Bundle {
  val id = Bits(params.id_width bits)
  val data = Bits(params.data_width bits)
}

class MemBusInvOp extends SpinalEnum {
  val flush, inv = newElement()
}

class MemBusInv(val params: MemBusParams) extends Bundle {
  val op = new MemBusInvOp
  val addr = UInt(params.addr_width bits)
}

class MemBusInvAck(val params: MemBusParams) extends Bundle {
  val with_data = Bool()
  val write_id = Bits(params.id_width bits)
}