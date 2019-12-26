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

  def defaults(csr: CSR) {
    csr.readers("mvendorid") := 0.U(csr.XLEN.W)
    csr.readers("marchid") := 0.U(csr.XLEN.W)
    csr.readers("mimpid") := 0.U(csr.XLEN.W)
    csr.readers("misa") := 2.U(2.W) ## 0.U((csr.XLEN-2-26).W) ## CSRHelper.buildExt("IMAC").U(26.W)
    csr.readers("mcounteren") := 0.U(csr.XLEN.W)
  }
}

class CSRPort(val XLEN: Int) extends Bundle {
  val rdata = Output(UInt(XLEN.W))

  val wdata = Input(UInt(XLEN.W))
  val write = Input(Bool())

  def connect(ano: CSRPort) {
    this.rdata := ano.rdata
    ano.wdata := this.wdata
    ano.write := this.write
  }
}

object CSRPort extends Bundle {
  def fromReg(XLEN: Int, reg: UInt): CSRPort = {
    val port = Wire(new CSRPort(XLEN))

    port.rdata := reg
    when(port.write) {
      reg := port.wdata
    }

    port
  }
}

/**
 * All CSR, mutable or immutable
 *
 * We are not declearing it as a bundle, because we are not passing it around as a whole,
 * and it's not an individual module as well
 */
class CSR(val XLEN: Int) {
  val readers = CSR.addrMap.values.map(_ match {
    case (name, _) => (name -> Wire(UInt(XLEN.W)))
  }).toMap

  val writers = CSR.addrMap.values.flatMap(_ match {
    case (name, true) => Some(name -> (Wire(UInt(XLEN.W)), Wire(Bool())))
    case (_, false) => None
  }).toMap

  def attach(name: String): CSRPort = {
    val port = Wire(Flipped(new CSRPort(XLEN)))

    readers.get(name).get := port.rdata
    port.write := writers.get(name).map(_._2).getOrElse(false.B)
    port.wdata := writers.get(name).map(_._1).getOrElse(DontCare)

    port
  }

  def const(name: String): UInt = {
    readers.get(name).get
  }
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
    0x302 -> ("medeleg", true),
    0x303 -> ("mideleg", true),
    0x304 -> ("mie", true),
    0x305 -> ("mtvec", true),
    0x306 -> ("mcounteren", false),
    0x320 -> ("mcountinhibit", true),
    0x340 -> ("mscratch", true),
    0x341 -> ("mepc", true),
    0x342 -> ("mcause", true),
    0x343 -> ("mtval", true),
    0x344 -> ("mip", true),
    0xB00 -> ("mcycle", true),
    0xB02 -> ("minstret", true)
  )

  /*
  // Some bit fields shall be perserved to be WPRI/WARL.
  val maskMap = Map(
    0x300 -> 0x88.U, // Having only M mode, only MPIE and MIE writable
    0x304 -> 0x333.U, // Only [U|S][S|T|E]IP writable (does we actually have them?)
    0x344 -> 0xBBB.U
  )
  */

  def gen(XLEN: Int, HART_ID: Int): (CSRWriter, CSR) = {
    val csr = new CSR(XLEN)
    val endpoint = Wire(Flipped(new CSRWriter(csr.XLEN)))

    val data = MuxLookup[UInt, UInt](
      endpoint.addr,
      0.U,
      addrMap.mapValues(value => value match {
        case (name, _) => csr.readers(name)
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

    for((d, e) <- csr.writers.values) {
      e := false.B
      d := DontCare
    }

    when(wcommit) {
      for((addr, (name, isMut)) <- addrMap) {
        if(isMut) {
          when(addr.U === endpoint.addr) {
            csr.writers(name)._1 := wdata
            csr.writers(name)._2 := true.B
          }
        }
      }
    }

    (endpoint, csr)
  }
}
