package core.cache

import chisel3._
import _root_.core._
import chisel3.util._

class Metadata(implicit val config: Config) extends Bundle {
  val valid = Bool()
  val tag = UInt(config.iCache.tagWidth.W)

  def hit(itag: UInt) = valid && tag === itag
}

class ICache(implicit val config: Config) extends MultiIOModule {
  val addr = IO(DecoupledIO(new PAddr))
  val flush = IO(Input(Bool()))

  val addrFlatten = addr.asUInt()
  val addrIdx = addrFlatten(config.iCache.offsetWidth + config.iCache.idxWidth - 1, config.iCache.offsetWidth)
  val addrTag = addrFlatten >> (config.iCache.offsetWidth + config.iCache.idxWidth)

  val FetchResult = Vec(config.fetchWidth, UInt(16.W))
  val fetched = IO(DecoupledIO(FetchResult))

  val metadata = Mem(
    config.iCache.rowPerAssoc,
    Vec(config.iCache.assoc, new Metadata)
  )
  val data = Mem(
    config.iCache.rowPerAssoc,
    Vec(config.iCache.assoc, Vec(config.iCache.rowInstr / config.fetchWidth, FetchResult)),
  )

  val flow = WireDefault(true.B)

  /* Stage 0: Read metadata, read data */
  val s0Valid = addr.valid
  val s0MetaReadout = metadata.read(addrIdx)
  val s0MetaHitmap = VecInit(s0MetaReadout.map(_.hit(addrTag)))
  val s0DataReadout = data.read(addrIdx)

  /* Stage 1: mux data */
  val s1MetaHitmap = RegEnable(s0MetaHitmap, flow)
  val s1Hit = s1MetaHitmap.asUInt().orR()
  val s1DataReadout = RegEnable(s0DataReadout, flow)
  val s1DataMux = Mux1H(s1MetaHitmap, s1DataReadout)
  val s1Addr = RegEnable(addr, flow)

  val s1AddrOffset = s1Addr.asUInt()(config.iCache.offsetWidth-1, 1)
  val s1DataSel = s1DataMux(s1AddrOffset)

  val s1Valid = RegEnable(s0Valid, flow)

  fetched.bits := s1DataSel
  fetched.valid := s1Valid && s1Hit

  when(!s1Hit && s1Valid) {
    // FIXME: fetch
  }

  when(s1Valid && fetched.ready) {
    flow := false.B
  }

  /* Flush logic */
  when(flush) {
    s1Valid := false.B
  }
}