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
class CSR(val XLEN: Int) {
  val csr = Map(
    "misa" -> RegInit(2.U(2.W) ## 0.U((XLEN-2-26).W) ## CSRHelper.buildExt("IMAC").U(26.W)),
    "mtvec" -> RegInit(0.U(XLEN.W))
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
    0x301 -> ("misa", false),
    0x305 -> ("mtvec", true)
  )

  def gen(XLEN: Int): (CSRWriter, CSR) = {
    val csr = new CSR(XLEN)
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
