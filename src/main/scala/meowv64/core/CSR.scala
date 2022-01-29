package meowv64.core

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._

object CSRHelper {
  def buildExt(exts: String): Int = {
    var ret = 0
    for (ext <- exts) {
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
    csr.readers("misa") := 2.U(2.W) ## 0
      .U((csr.XLEN - 2 - 26).W) ## CSRHelper.buildExt("IMAFDCSU").U(26.W)
    csr.readers("mcounteren") := 0.U(csr.XLEN.W)
    csr.readers("scounteren") := 0.U(csr.XLEN.W)
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

object CSRPort {
  def fromReg[T <: Data](XLEN: Int, reg: T): CSRPort = {
    val port = Wire(new CSRPort(XLEN))

    port.rdata := reg.asUInt
    when(port.write) {
      reg := port.wdata.asTypeOf(reg)
    }

    port
  }
}

/** All CSR, mutable or immutable
  *
  * We are not declearing it as a bundle, because we are not passing it around
  * as a whole, and it's not an individual module as well
  */
class CSR(val XLEN: Int) {
  val readers = CSR.addrMap.values
    .map(_ match {
      case (name, _) => (name -> Wire(UInt(XLEN.W)))
    })
    .toMap

  val writers = CSR.addrMap.values
    .flatMap(_ match {
      case (name, true) => Some(name -> ((Wire(UInt(XLEN.W)), Wire(Bool()))))
      case (_, false)   => None
    })
    .toMap

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

class VState(implicit coredef: CoreDef) extends Bundle {
  val vl = UInt(coredef.XLEN.W)
  val vtype = new VType
}

class CSRWriter(implicit coredef: CoreDef) extends Bundle {
  // csr instructions
  val addr = Output(UInt(12.W))
  val rdata = Input(UInt(coredef.XLEN.W))
  val wdata = Output(UInt(coredef.XLEN.W))
  val write = Output(Bool())

  // vsetvl
  val currentVState = Input(new VState)
  val updateVState = Valid(new VState)
}

object CSR {
  // Is mutable on the second parameter
  val addrMap = Map(
    // Float
    0x001 -> (("fflags", true)),
    0x002 -> (("frm", true)),
    0x003 -> (("fcsr", true)),

    // Vector
    0xc20 -> (("vl", false)),
    0xc21 -> (("vtype", false)),
    0xc22 -> (("vlenb", false)),

    // Machine
    0xf11 -> (("mvendorid", false)),
    0xf12 -> (("marchid", false)),
    0xf13 -> (("mimpid", false)),
    0xf14 -> (("mhartid", false)),
    0x300 -> (("mstatus", true)),
    0x301 -> (("misa", false)),
    0x302 -> (("medeleg", true)),
    0x303 -> (("mideleg", true)),
    0x304 -> (("mie", true)),
    0x305 -> (("mtvec", true)),
    0x306 -> (("mcounteren", false)), // TODO: impl counter
    0x320 -> (("mcountinhibit", true)),
    0x340 -> (("mscratch", true)),
    0x341 -> (("mepc", true)),
    0x342 -> (("mcause", true)),
    0x343 -> (("mtval", true)),
    0x344 -> (("mip", true)),
    0xb00 -> (("mcycle", true)),
    0xb02 -> (("minstret", true)),

    // Supervisor
    0x100 -> (("sstatus", true)),
    // No N extension
    // 0x102 -> (("sedeleg", true)),
    // 0x103 -> (("sideleg", true)),
    0x104 -> (("sie", true)),
    0x105 -> (("stvec", true)),
    0x106 -> (("scounteren", false)),
    0x140 -> (("sscratch", true)),
    0x141 -> (("sepc", true)),
    0x142 -> (("scause", true)),
    0x143 -> (("stval", true)),
    0x144 -> (("sip", true)),
    0x180 -> (("satp", true)),

    // User
    0xc00 -> (("cycle", false)),
    0xc01 -> (("time", false)),
    0xc02 -> (("instret", false))
  )

  /*
  // Some bit fields shall be preserved to be WPRI/WARL.
  val maskMap = Map(
    0x300 -> 0x88.U, // Having only M mode, only MPIE and MIE writable
    0x304 -> 0x333.U, // Only [U|S][S|T|E]IP writable (does we actually have them?)
    0x344 -> 0xBBB.U
  )
   */

  def gen(XLEN: Int, HART_ID: Int)(implicit
      coredef: CoreDef
  ): (CSRWriter, CSR) = {
    val csr = new CSR(XLEN)
    val endpoint = Wire(Flipped(new CSRWriter()))

    val data = Mux1H(
      addrMap
        .mapValues(value =>
          value match {
            case (name, _) => csr.readers(name)
          }
        )
        .toSeq
        .map(_ match { case (addr, r) => (endpoint.addr === addr.U, r) })
    )

    endpoint.rdata := data

    for ((d, e) <- csr.writers.values) {
      e := false.B
      d := DontCare
    }

    when(endpoint.write) {
      for ((addr, (name, isMut)) <- addrMap) {
        if (isMut) {
          when(addr.U === endpoint.addr) {
            csr.writers(name)._1 := endpoint.wdata
            csr.writers(name)._2 := true.B
          }
        }
      }
    }

    (endpoint, csr)
  }
}

class Status(implicit val coredef: CoreDef) extends Bundle {
  val sd = Bool()
  val WPRI1 = UInt((coredef.XLEN - 37).W)

  // These bits are not supported, so they are effectively WPRI bits
  val sxl = UInt(2.W)
  val uxl = UInt(2.W)
  val WPRI2 = UInt(9.W)
  val tsr = Bool()
  val tw = Bool()
  val tvm = Bool()

  val mxr = Bool()
  val sum = Bool()
  val mprv = Bool()

  val xs = UInt(2.W)
  val fs = UInt(2.W)

  val mpp = UInt(2.W)
  val WPRI3 = UInt(2.W)
  val spp = Bool()

  val mpie = Bool()
  val WPRI4 = Bool()
  val spie = Bool()
  val upie = Bool()

  val mie = Bool()
  val WPRI5 = Bool()
  val sie = Bool()
  val uie = Bool()
}

object Status {
  def hardwired(base: Status)(implicit coredef: CoreDef) = {
    val result = Wire(new Status)
    result := DontCare

    // SD = ((FS==11) OR (XS==11))
    result.sd := base.fs === 3.U
    // No XS
    result.xs := 0.U

    // Cannot change XLEN
    result.sxl := 2.U
    result.uxl := 2.U

    result
  }

  def empty(implicit coredef: CoreDef) = 0.U.asTypeOf(new Status)

  def mwpri(implicit coredef: CoreDef) = BigInt(
    "0" +
      "1" * (coredef.XLEN - 37) +
      "0000" +
      "1" * 9 +
      "0" * 12 +
      "11001000100",
    2
  ).U

  def mmask(implicit coredef: CoreDef) = BigInt(
    "0" + // SD not supported
      "0" * (coredef.XLEN - 37) + // WPRI
      "0" * 4 + // SXL and UXL not supported
      "0" * 9 + // WPRI
      "1" * 6 + // TSR - MPRV
      "0011" + // XS & FS
      "11001" + // xPP
      "10111011" // xP?IE
    ,
    2
  ).U

  def swpri(implicit coredef: CoreDef) = BigInt(
    "0" +
      "1" * (coredef.XLEN - 35) +
      "00" +
      "1" * 12 +
      "00100001111011001100",
    2
  ).U

  def smask(implicit coredef: CoreDef) = BigInt(
    "0" + // SD not supported
      "0" * (coredef.XLEN - 35) + // WPRI
      "0" * 2 + // SXL and UXL not supported
      "0" * 12 + // WPRI
      "11" + // MXR, SUM
      "00000" + // WPRI, XS & FS
      "00001" + // xPP
      "00110011" // xP?IE
    ,
    2
  ).U
}

class IntConfGroup extends Bundle {
  val m = Bool()
  val h = Bool() // Not supported
  val s = Bool()
  val u = Bool() // Not supported
}

object IntConfGroup {
  def mwpri = BigInt("0100", 2).U(4.W)
  def mmask(pending: Boolean) = if (pending) {
    BigInt("0010", 2).U(4.W) // Cannot set MxIP in MIP
  } else {
    BigInt("1010", 2).U(4.W)
  }
  def swpri = BigInt("1100", 2).U(4.W)
  def smask(pending: Boolean) = if (pending) {
    BigInt("0000", 2).U(4.W) // Cannot set SxIP in SIP
  } else {
    BigInt("0010", 2).U(4.W)
  }

  def hardwired = {
    val result = Wire(new IntConfGroup)
    result := DontCare
    result.u := false.B
    result
  }

  def empty = 0.U.asTypeOf(new IntConfGroup)
}

class IntConf(implicit val coredef: CoreDef) extends Bundle {
  val WPRI1 = UInt((coredef.XLEN - 12).W)
  val external = new IntConfGroup
  val timer = new IntConfGroup
  val software = new IntConfGroup
}

object IntConf {
  def mwpri(implicit coredef: CoreDef) = (
    BigInt("1" * (coredef.XLEN - 12), 2).U((coredef.XLEN - 12).W)
      ## IntConfGroup.mwpri
      ## IntConfGroup.mwpri
      ## IntConfGroup.mwpri
  )

  def swpri(implicit coredef: CoreDef) = (
    BigInt("1" * (coredef.XLEN - 12), 2).U((coredef.XLEN - 12).W)
      ## IntConfGroup.swpri
      ## IntConfGroup.swpri
      ## IntConfGroup.swpri
  )

  def mmask(pending: Boolean)(implicit coredef: CoreDef) = (
    BigInt("0" * (coredef.XLEN - 12), 2).U((coredef.XLEN - 12).W)
      ## IntConfGroup.mmask(pending)
      ## IntConfGroup.mmask(pending)
      ## IntConfGroup.mmask(pending)
  )

  def smask(pending: Boolean)(implicit coredef: CoreDef) = (
    BigInt("0" * (coredef.XLEN - 12), 2).U((coredef.XLEN - 12).W)
      ## IntConfGroup.smask(pending)
      ## IntConfGroup.smask(pending)
      ## IntConfGroup.smask(pending)
  )

  def empty(implicit coredef: CoreDef) = {
    val result = Wire(new IntConf)
    result.WPRI1 := 0.U
    result.external := IntConfGroup.empty
    result.timer := IntConfGroup.empty
    result.software := IntConfGroup.empty
    result
  }

  def hardwired(implicit coredef: CoreDef) = {
    val result = Wire(new IntConf)
    result.WPRI1 := DontCare
    result.external := IntConfGroup.hardwired
    result.timer := IntConfGroup.hardwired
    result.software := IntConfGroup.hardwired
    result
  }
}

object SatpMode extends ChiselEnum {
  val bare = Value(0.U(4.W))
  val sv39 = Value(8.U(4.W))
  val sv48 = Value(9.U(4.W))
}

class Satp extends Bundle {
  val mode = SatpMode()
  val asic = UInt(16.W)
  val ppn = UInt(44.W)

  def port = {
    val port = Wire(new CSRPort(64))
    port.rdata := this.asTypeOf(port.rdata)
    val casted = port.wdata.asTypeOf(this)
    val modeValid = casted.mode.isValid

    when(port.write) {
      asic := casted.asic
      ppn := casted.ppn
      when(modeValid) {
        mode := casted.mode
      }
    }

    port
  }
}

object Satp {
  def empty = {
    val ret = Wire(new Satp)
    ret.asic := 0.U
    ret.ppn := 0.U
    ret.mode := SatpMode.bare
    ret
  }
}
