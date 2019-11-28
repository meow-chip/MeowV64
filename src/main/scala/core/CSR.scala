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
    "mvendorid" -> RegInit(0.U(XLEN.W)),
    "marchid" -> RegInit(0.U(XLEN.W)),
    "mimpid" -> RegInit(0.U(XLEN.W)),
    "mhartid" -> RegInit(HART_ID.U(XLEN.W)),
    "mstatus" -> RegInit(0.U(XLEN.W)), // Only have M mode, everything hardwired/initialized to 0
    "misa" -> RegInit(2.U(2.W) ## 0.U((XLEN-2-26).W) ## CSRHelper.buildExt("IMAC").U(26.W)),
    "mie" -> RegInit(0.U(XLEN.W)),
    "mtvec" -> RegInit(0.U(XLEN.W)),
    "mcounteren" -> RegInit(0.U(XLEN.W)),
    "mscratch" -> RegInit(0.U(XLEN.W)),
    "mepc" -> RegInit(0.U(XLEN.W)),
    "mcause" -> RegInit(0.U(XLEN.W)),
    "mtval" -> RegInit(0.U(XLEN.W)),
    "mip" -> RegInit(0.U(XLEN.W))
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
    0x304 -> ("mie", true),
    0x305 -> ("mtvec", true),
    0x306 -> ("mcounteren", false),
    0x340 -> ("mscratch", true),
    0x341 -> ("mepc", true),
    0x342 -> ("mcause", true),
    0x343 -> ("mtval", true),
    0x344 -> ("mip", true)
  )

  def gen(XLEN: Int, HART_ID: Int): (CSRWriter, CSR) = {
    val csr = new CSR(XLEN, HART_ID)
    val endpoint = Wire(Flipped(new CSRWriter(csr.XLEN)))

    val data = MuxLookup[UInt, UInt](
      endpoint.addr,
      0.U,
      addrMap.mapValues(value => value match {
        case (name, _) => csr.csr(name)
      }).toSeq.map(_ match { case (addr, r) => (addr.U, r) })
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
      for((addr, (name, isMut)) <- addrMap)
        if(isMut)
          when(addr.U === endpoint.addr) {
            csr.csr(name) := wdata
          }
    }

    (endpoint, csr)
  }
}
