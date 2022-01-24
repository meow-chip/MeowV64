package meowv64.instr

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import meowv64.core._

/** BHT prediction type: branch taken, branch not taken, or missing in BHT
  */
object BHTPrediction extends ChiselEnum {
  val taken, notTaken, missed = Value
}

class BHTSlot(implicit val coredef: CoreDef) extends Bundle {
  val OFFSET_WIDTH = log2Ceil(coredef.L1I.TRANSFER_WIDTH)
  val INDEX_WIDTH = log2Ceil(coredef.BHT_SIZE)

  val TAG_WIDTH = coredef.VADDR_WIDTH - OFFSET_WIDTH - INDEX_WIDTH

  val tag = UInt(TAG_WIDTH.W)
  val valid = Bool()

  /** Branch history register
    */
  val bhr = UInt(coredef.BHR_WIDTH.W)

  /** BHT history counter
    */
  val history = Vec(1 << coredef.BHR_WIDTH, UInt(coredef.BHT_WIDTH.W))

  /** Target address if branch is taken.
    *
    * If necessary, store offset instead of target address.
    */
  val targetAddress = UInt(coredef.XLEN.W)

  /** Compute BPUResult
    */
  def taken(tag: UInt) = {
    val ret = Wire(new BPUResult())
    ret.valid := valid && this.tag === tag
    ret.bhr := bhr
    ret.history := history
    ret.targetAddress := targetAddress
    ret
  }
}

class BPUResult(implicit val coredef: CoreDef) extends Bundle {
  val OFFSET_WIDTH = log2Ceil(coredef.L1I.TRANSFER_WIDTH / 8)
  val INDEX_WIDTH = log2Ceil(coredef.BHT_SIZE)
  val TAG_WIDTH = coredef.VADDR_WIDTH - OFFSET_WIDTH - INDEX_WIDTH

  /** This is a valid prediction
    */
  val valid = Bool()

  /** Branch history register
    */
  val bhr = UInt(coredef.BHR_WIDTH.W)

  /** BHT history counter
    */
  val history = Vec(1 << coredef.BHR_WIDTH, UInt(coredef.BHT_WIDTH.W))

  /** Predicted target address.
    *
    * For BRANCH/JAL instructions, this is pc + imm. For JALR(RET) instruction,
    * this comes from RAS.
    */
  val targetAddress = UInt(coredef.XLEN.W)

  // predict by msb: <1/2 not taken, >1/2 taken
  def prediction = Mux(
    valid,
    Mux(
      history(bhr)(coredef.BHT_WIDTH - 1),
      BHTPrediction.taken,
      BHTPrediction.notTaken
    ),
    BHTPrediction.missed
  )

  def up(tag: UInt): BHTSlot = {
    val ret = Wire(new BHTSlot())

    ret.valid := true.B
    ret.tag := tag
    ret.targetAddress := targetAddress
    ret.history := this.history
    ret.bhr := this.bhr ## true.B

    when(this.valid) {
      // saturating add
      ret.history(this.bhr) := Mux(
        this.history(this.bhr).andR(),
        (-1).S(coredef.BHT_WIDTH.W).asUInt,
        this.history(this.bhr) + 1.U
      )
    }.otherwise {
      for (i <- 0 until (1 << coredef.BHR_WIDTH)) {
        ret.history(i) := 1.U(1.W) ## 0.U((coredef.BHT_WIDTH - 1).W)
      }
    }

    ret
  }

  def down(tag: UInt): BHTSlot = {
    val ret = Wire(new BHTSlot())

    ret.valid := true.B
    ret.tag := tag
    ret.targetAddress := targetAddress
    ret.history := this.history
    ret.bhr := this.bhr ## false.B

    when(this.valid) {
      // saturating sub
      ret.history(this.bhr) := Mux(
        this.history(this.bhr).orR(),
        this.history(this.bhr) - 1.U,
        0.U
      )
    }.otherwise {
      for (i <- 0 until (1 << coredef.BHR_WIDTH)) {
        ret.history(i) := 0.U(1.W) ## (-1).S((coredef.BHT_WIDTH - 1).W).asUInt
      }
    }

    ret
  }

  /** Update BHT prediction history
    *
    * @param taken
    * @param tag
    * @return
    */
  def update(taken: Bool, tag: UInt) = Mux(taken, this.up(tag), this.down(tag))
}

object BPUResult {
  def empty(implicit coredef: CoreDef) = {
    val res = Wire(new BPUResult)
    res.valid := false.B
    res.bhr := 0.U
    for (i <- 0 until (1 << coredef.BHR_WIDTH)) {
      res.history(i) := 0.U
    }
    res.targetAddress := 0.U

    res
  }

}

/** Branch prediction unit. It only considers branch instructions and jal.
  */
class BPU(implicit val coredef: CoreDef) extends Module {
  val toFetch = IO(new Bundle {

    /** the address (pc) of the query branch */
    val pc =
      Input(Valid(UInt(coredef.VADDR_WIDTH.W)))
    // for each possible branch instruction
    // return a BPUResult
    // it has one cycle delay
    val results = Output(
      Vec(coredef.L1I.TRANSFER_WIDTH / Const.INSTR_MIN_WIDTH, new BPUResult)
    )
  })

  val toExec = IO(new Bundle {

    /** Update BPU based on execution result
      */
    val valid = Input(Bool())
    val lpc = Input(UInt(coredef.XLEN.W)) // Only register on the last slot
    val hist = Input(new BPUResult)
    val taken = Input(Bool())
  })

  val INLINE_COUNT = coredef.L1I.TRANSFER_WIDTH / Const.INSTR_MIN_WIDTH
  val OFFSET_WIDTH = log2Ceil(coredef.L1I.TRANSFER_WIDTH / 8)
  val INDEX_WIDTH = log2Ceil(coredef.BHT_SIZE)
  val INDEX_OFFSET_WIDTH = OFFSET_WIDTH + INDEX_WIDTH
  val TAG_WIDTH = coredef.VADDR_WIDTH - OFFSET_WIDTH - INDEX_WIDTH

  def getIndex(addr: UInt) = addr(INDEX_OFFSET_WIDTH - 1, OFFSET_WIDTH)
  def getTag(addr: UInt) = addr(coredef.VADDR_WIDTH - 1, INDEX_OFFSET_WIDTH)
  def getOffset(addr: UInt) =
    addr(OFFSET_WIDTH - 1, log2Ceil(Const.INSTR_MIN_WIDTH / 8))
  def toAligned(addr: UInt) = getTag(addr) ## getIndex(addr) ## 0.U(
    OFFSET_WIDTH.W
  ) // The input address should be aligned anyway

  val store = Mem(coredef.BHT_SIZE, Vec(INLINE_COUNT, new BHTSlot))

  val reseting = RegInit(true.B)
  val resetCnt = RegInit(0.U(log2Ceil(coredef.BHT_SIZE).W))

  // Prediction part
  val readout = store.read(getIndex(toFetch.pc.bits))
  val tag = getTag(toFetch.pc.bits)
  val pipeReadout = RegNext(readout)
  val pipeTag = RegNext(tag)
  when(RegNext(toFetch.pc.valid)) {
    toFetch.results := VecInit(pipeReadout.map(_.taken(pipeTag)))
  }.otherwise {
    toFetch.results := 0.U.asTypeOf(toFetch.results)
  }

  // valid is stale when in reset state
  when(reseting) {
    for (res <- toFetch.results) {
      res.valid := false.B
    }
  }

  // Update part

  // Write bypass: if two updates to the same entry is close,
  // the state might be stale and the first update is silently overridden.
  // Record the history of recent writes, and bypass the state if available.
  val wrBypassPc = RegInit(
    VecInit.fill(coredef.BPU_WRITE_BYPASS_COUNT)(0.U(coredef.XLEN.W))
  )
  val wrBypassState = RegInit(
    VecInit.fill(coredef.BPU_WRITE_BYPASS_COUNT)(BPUResult.empty)
  )
  val wrBypassWriteIdx = RegInit(
    0.U(log2Ceil(coredef.BPU_WRITE_BYPASS_COUNT).W)
  )

  val wrBypassHitVec = wrBypassPc
    .zip(wrBypassState)
    .map({ case (pc, state) => pc === toExec.lpc && state.valid })
  val wrBypassHit = wrBypassHitVec.reduce(_ || _)
  val wrBypassHitIdx = PriorityEncoder(wrBypassHitVec)

  val updateTag = getTag(toExec.lpc)
  val updateOffset = getOffset(toExec.lpc)
  assert(
    updateOffset.getWidth == log2Ceil(
      coredef.L1I.TRANSFER_WIDTH / Const.INSTR_MIN_WIDTH
    )
  )

  // Consider write bypass
  val history = WireInit(toExec.hist)
  when(wrBypassHit) {
    val bypass = wrBypassState(wrBypassHitIdx)
    history.bhr := bypass.bhr
    history.history := bypass.history
  }

  val updated = history.update(toExec.taken, updateTag)
  val updateMask = UIntToOH(updateOffset).asBools()

  val we = updateMask.map(bit => {
    val ret = WireDefault(bit)
    when(!toExec.valid) {
      ret := false.B
    }

    when(reseting) {
      ret := true.B
    }

    ret
  })

  val init = Wire(new BHTSlot)
  for (i <- 0 until (1 << coredef.BHR_WIDTH)) {
    init.history(i) := 0.U
  }
  init.bhr := 0.U
  init.tag := 0.U
  init.targetAddress := 0.U
  init.valid := false.B

  val waddr = Mux(reseting, resetCnt, getIndex(toExec.lpc))
  val data = VecInit(
    Seq.fill(coredef.L1I.TRANSFER_WIDTH / Const.INSTR_MIN_WIDTH)(
      Mux(reseting, init, updated)
    )
  )

  store.write(waddr, data, we)

  // Save to write bypass buffer
  when(toExec.valid) {
    when(wrBypassHit) {
      // update in-place
      wrBypassState(wrBypassHitIdx) := updated
    }.otherwise {
      wrBypassPc(wrBypassWriteIdx) := toExec.lpc
      wrBypassState(wrBypassWriteIdx) := updated

      when(wrBypassWriteIdx === (coredef.BPU_WRITE_BYPASS_COUNT - 1).U) {
        wrBypassWriteIdx := 0.U
      }.otherwise {
        wrBypassWriteIdx := wrBypassWriteIdx + 1.U
      }
    }
  }

  when(reseting) {
    resetCnt := resetCnt +% 1.U
    when(resetCnt.andR()) {
      reseting := false.B
    }
  }
}
