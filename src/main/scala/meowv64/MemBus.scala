package meowv64

import spinal.core._
import spinal.lib._

case class MemBusParams(
  val addr_width: Int,
  val data_width: Int,
  val id_width: Int,
);

/**
  * Internal memory bus. This is heavely inspired by the BMB(Banana memory bus).
  *
  * @param isInstr: If this memory bus's master is an I$
  */
class MemBus(val params: MemBusParams, val is_instr: Boolean) extends Bundle with IMasterSlave {
  val cmd = Stream(new MemBusCmd(params, is_instr))
  val uplink = Stream(new MemBusUplink(params))
  val downlink = if(is_instr) null else Stream(new MemBusDownlink(params))
  val inv = if(is_instr) null else Stream(new MemBusInv(params))
  val ack = if(is_instr) null else new MemBusInvAck(params)

  override def asMaster(): Unit = {
    master(cmd, downlink)
    slave(uplink, inv)
    out(ack)
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
class MemBusCmd(val params: MemBusParams, val is_instr: Boolean) extends Bundle {
  val id = Bits(params.id_width bits)

  val op = if(is_instr) null else new MemBusOp
  val subop = if(is_instr) null else Bits(5 bits)

  // Keyword first sematic, beat count is always determined by cache line width
  val addr = UInt(params.addr_width bits)
}

// Master -> Slave
class MemBusDownlink(val params: MemBusParams) extends Bundle {
  val id = Bits(params.id_width bits)
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