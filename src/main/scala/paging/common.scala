package paging

import chisel3._
import chisel3.util._
import _root_.core.CoreDef

class PTE(implicit val coredef: CoreDef) extends Bundle {
  val ppn = UInt(54.W)
  val RESERVED1 = UInt(2.W)
  val d = Bool()
  val a = Bool()
  val g = Bool()
  val u = Bool()
  val x = Bool()
  val w = Bool()
  val r = Bool()
  val v = Bool()

  def intermediate = v && !w && !r && !x
  def valid = v && !((!r) && x)
}

object PTE {
  def empty(implicit coredef: CoreDef) = {
    val ret = Wire(new PTE)
    ret := DontCare
    ret.v := false.B
    ret
  }
}

class TLBEntry(implicit val coredef: CoreDef) extends Bundle {
  // We have at most 4 vpn segments (Sv48)
  val MAX_VPN_SEG = 4

  val vpn = UInt(coredef.vpnWidth.W)
  val ppn = UInt(coredef.ppnWidth.W)

  val level = UInt(log2Ceil(MAX_VPN_SEG).W)

  val d = Bool()
  val a = Bool()
  val u = Bool()

  val x = Bool()
  val w = Bool()
  val r = Bool()

  val v = Bool()

  def intermediate() = v && !w && !r && !x
  def hit(req: UInt) = {
    val qvpn = req.head(coredef.vpnWidth)
    var result = v
    for(i <- 0 until MAX_VPN_SEG) {
      val ignore = i.U > level
      val base = 9 * i
      result = Mux(
        ignore,
        result,
        result && vpn(base+8, base) === qvpn(base+8, base)
      )
    }
    result
  }
}

object TLBEntry {
  def empty(implicit coredef: CoreDef) = {
    val ret = Wire(new TLBEntry)
    ret := DontCare
    ret.v := false.B
    ret
  }

  def fromPTE(vpn: UInt, level: UInt, pte: PTE)(implicit coredef: CoreDef) = {
    val ret = Wire(new TLBEntry)
    ret.vpn := vpn
    ret.level := level

    ret.ppn := pte.ppn

    ret.d := pte.d
    ret.a := pte.a
    ret.u := pte.u

    ret.r := pte.r
    ret.w := pte.w
    ret.x := pte.x

    ret.v := pte.v

    ret
  }
}

class TLBExt(implicit val coredef: CoreDef) extends Bundle {
  val req = DecoupledIO(UInt(coredef.vpnWidth.W))
  val resp = Input(new PTE)
  val fault = Input(Bool())
  val level = Input(UInt(2.W)) // TODO: change into config
}

class PTWExt(implicit val coredef: CoreDef) extends Bundle {
  val req = DecoupledIO(UInt(coredef.PADDR_WIDTH.W))
  val resp = Input(UInt(coredef.XLEN.W))
}
