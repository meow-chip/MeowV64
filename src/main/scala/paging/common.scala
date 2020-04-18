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
  def valid = v && !((!r) && w)
  def misaligned(level: UInt)(implicit coredef: CoreDef) = {
    val MAX_SEG = coredef.vpnWidth / 9
    // Current at level, we have MAX_SEG segments
    val segMisaligned = Wire(Vec(MAX_SEG-1, Bool()))
    for(i <- (0 until MAX_SEG-1)) {
      segMisaligned(i) := ppn(i*9+8, i*9).orR
    }

    MuxLookup(
      level,
      false.B, // If level == MAX_SEG - 1, then this is not a super page
      (0 until (MAX_SEG - 1)).map(i => i.U -> segMisaligned.asUInt()(MAX_SEG-2-i, 0).orR)
    )
  }
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
      val base = coredef.vpnWidth - 9 * (i+1)
      result = Mux(
        ignore,
        result,
        result && vpn(base+8, base) === qvpn(base+8, base)
      )
    }
    result
  }
  
  def fromVPN(vpn: UInt) = {
    val VPN_SEG = coredef.vpnWidth / 9
    Mux1H(
      (0 until VPN_SEG).map(
        i => (level === i.U) -> (if(i == VPN_SEG - 1) {
          ppn
        } else {
          ppn(coredef.ppnWidth-1, (VPN_SEG - 1 - i) * 9) ## vpn((VPN_SEG - 1 - i) * 9 - 1, 0)
        })
      )
    )
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
