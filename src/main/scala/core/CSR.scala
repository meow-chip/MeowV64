package core

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum

object CSRHelper {
  def buildExt(exts: String): Int = {
    var ret = 0
    for(ext <- exts) {
      assume(ext != 'G', "G in misa is reserved")

      val shift = ext - 'A'
      ret |= 1 << shift
    }

    ret
  }
}

/**
 * All CSR, mutable or immutable
 *
 * We are not declearing it as a bundle, because we are not passing it around as a whole,
 * and it's not an individual module as well
 */
class CSR(val XLEN: Int, HART_ID: Int) {
  val csr = Map(
    "mvendorid" -> 0.U(XLEN.W),
    "marchid" -> 0.U(XLEN.W),
    "mimpid" -> 0.U(XLEN.W),
    "mhartid" -> HART_ID.U(XLEN.W),
    "mstatus" -> RegInit(0.U(XLEN.W)), // Only have M mode, everything hardwired/initialized to 0
    "misa" -> RegInit(2.U(2.W) ## 0.U((XLEN-2-26).W) ## CSRHelper.buildExt("IMAC").U(26.W)),
    "mie" -> RegInit(0.U(XLEN.W)),
    "mtvec" -> RegInit(0.U(XLEN.W)),
    "mcounteren" -> RegInit(0.U(XLEN.W)),
    "mscratch" -> RegInit(0.U(XLEN.W)),
    "mepc" -> RegInit(0.U(XLEN.W)),
    "mcause" -> RegInit(0.U(XLEN.W)),
    "mtval" -> RegInit(0.U(XLEN.W)),
    "mip" -> RegInit(0.U(XLEN.W)),
    "mcycle" -> 0.U(64.W) // this should not be taken
  )
}

object CSROp extends ChiselEnum {
  val rw, rs, rc = Value
}

class CSRWriter(val XLEN: Int) extends Bundle {
  val addr = Output(UInt(12.W))
  val op = Output(CSROp())
  val rdata = Input(UInt(XLEN.W))
  val wdata = Output(UInt(XLEN.W))
}

object CSR {
  // Is mutable on the second parameter
  val addrMap = Map(
    0xF11 -> ("mvendorid", false),
    0xF12 -> ("marchid", false),
    0xF13 -> ("mimpid", false),
    0xF14 -> ("mhartid", false),
    0x300 -> ("mstatus", true),
    0x301 -> ("misa", false),
    // m[e|i]deleg should not exist on M mode only machine
    0x304 -> ("mie", true),
    0x305 -> ("mtvec", true),
    0x306 -> ("mcounteren", false),
    0x340 -> ("mscratch", true),
    0x341 -> ("mepc", true),
    0x342 -> ("mcause", true),
    0x343 -> ("mtval", true),
    0x344 -> ("mip", true),
    0xB00 -> ("mcycle", true)
  )

  // Some bit fields shall be perserved to be WPRI/WARL.
  val maskMap = Map(
    0x300 -> 0x88.U, // Having only M mode, only MPIE and MIE writable
    0x304 -> 0x333.U, // Only [U|S][S|T|E]IP writable (does we actually have them?)
    0x344 -> 0xBBB.U
  )

  def gen(XLEN: Int, HART_ID: Int): (CSRWriter, CSR, CounterPort) = {
    val csr = new CSR(XLEN, HART_ID)
    val endpoint = Wire(Flipped(new CSRWriter(csr.XLEN)))
    val counterport = Wire(new CounterPort)
    counterport.valid := false.B
    counterport.op := DontCare
    counterport.wdata := DontCare

    val data = MuxLookup[UInt, UInt](
      endpoint.addr,
      0.U,
      addrMap.mapValues(value => value match {
        case (name, _) => csr.csr(name)
      }).toSeq.map(_ match { case (addr, r) => (addr.U, r) })
    )

    val mask = MuxLookup[UInt, UInt](
      endpoint.addr,
      0xFFFF.S(XLEN.W).asUInt, // sign extended
      maskMap.toSeq.map(_ match { case (addr, m) => (addr.U, m) })
    )

    endpoint.rdata := data
    val wdata = Wire(UInt(csr.XLEN.W))
    val wcommit = Wire(Bool())
    wdata := DontCare
    wcommit := false.B

    switch(endpoint.op) {
      is(CSROp.rw) {
        wdata := endpoint.wdata
        wcommit := true.B
      }

      is(CSROp.rs) {
        wdata := data | endpoint.wdata
        wcommit := true.B
      }

      is(CSROp.rc) {
        wdata := data & (~endpoint.wdata)
        wcommit := true.B
      }
    }

    when(wcommit) {
      for((addr, (name, isMut)) <- addrMap) {
        if(addr == 0xB00) {
          counterport.valid := true.B
          counterport.op := endpoint.op
          counterport.wdata := endpoint.wdata
          endpoint.rdata := counterport.rdata
        } else if(isMut)
          when(addr.U === endpoint.addr) {
            csr.csr(name) := wdata & mask
        }
      }
    }

    (endpoint, csr, counterport)
  }
}
