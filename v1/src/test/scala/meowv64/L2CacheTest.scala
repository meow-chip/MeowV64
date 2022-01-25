package meowv64

import meowv64.core._
import meowv64.util._
import meowv64.cache._
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Random
import scala.collection.mutable.HashMap
import meowv64.interrupt.CLINTMapping
import meowv64.interrupt.PLICMapping

object CacheTestDef extends CoreDef {

  override val HART_ID: Int = 0

}

object L1DCacheTestDef
    extends {
      val ADDR_WIDTH: Int = 48
      val ASSOC: Int = 2
      val LINE_BYTES: Int = 16
      val SIZE_BYTES: Int = 128
      val TRANSFER_WIDTH: Int = 64
      val XLEN: Int = 64

      val WRITE_BUF_DEPTH = 2
    }
    with L1DOpts;

object L2CacheTestDef
    extends {
      val ADDR_WIDTH: Int = 48
      val ASSOC: Int = 4
      val CORE_COUNT: Int = 1
      val LINE_BYTES: Int = 16
      val TRANSFER_WIDTH: Int = 0 // Actually ignored
      val SIZE_BYTES: Int = 256
      val WB_DEPTH: Int = 4
      val XLEN: Int = 64
      val MMIO = Seq(
        CLINTMapping,
        PLICMapping
      )
    }
    with L2Opts

class WrappedL2(l1do: L1DOpts, l2o: L2Opts, implicit val coredef: CoreDef)
    extends Module {
  val io = IO(new Bundle {
    val reader = Flipped(new DCReader())
    val writer = Flipped(new DCWriter(l1do))
  })

  val mem = Module(new AXIMem(None, L2CacheTest.RAM_SIZE, 48))

  val l2 = Module(new L2Cache(l2o))
  val l1d = Module(new L1DC(l1do))

  l2.directs(0) <> L1UCPort.empty(l1do)
  l2.ic(0) <> L1ICPort.empty(l1do)
  l2.dc(0) <> l1d.toL2

  // tie to zero
  l1d.ptw.req.valid := false.B
  l1d.ptw.req.bits := DontCare
  for (mmio <- l2.mmio) {
    mmio.req.ready := false.B
    mmio.resp.valid := false.B
    mmio.resp.bits := 0.U
  }

  io.reader <> l1d.mr
  io.writer <> l1d.w

  l2.axi <> mem.io.axi
}

class L2CacheTest(dut: WrappedL2, seed: Long, len: Int) {
  println(s"Testing cache with seed: ${seed} for ${len} cycles")

  val rng = new Random(seed)

  class Req(
      val isRead: Boolean,
      val addr: Long,
      val wdata: BigInt,
      val lg2Len: Int // 0-3 means 1, 2, 4, 8
  )

  /** Align address to 8 byte boundary
    */
  def align(input: Long): Long = (input >> 3) << 3

  def genReq(rng: Random): Req = {
    val lg2Len = Math.abs(rng.nextInt()) % 4
    // align addr to lg2Len boundary
    new Req(
      isRead = rng.nextFloat() > L2CacheTest.W_RATIO,
      addr =
        ((Math.abs(rng.nextLong()) % L2CacheTest.RAM_SIZE) >> lg2Len) << lg2Len,
      wdata = Math.abs(rng.nextLong()),
      lg2Len = lg2Len
    )
  }

  var cnt = 0

  // addresses are aligned here
  val ref = new HashMap[Long, BigInt]()

  def nextReq(rng: Random, ref: HashMap[Long, BigInt]): Req = {
    val ret = genReq(rng)

    // found in memory
    val aligned = align(ret.addr)
    val valid = ref.contains(aligned)

    if (valid) {
      return ret
    } else {
      // generate write request
      return new Req(
        isRead = false,
        addr = aligned,
        wdata = ret.wdata,
        lg2Len = 3 // write 8 bytes
      )
    }

    ret
  }

  var cur: Req = nextReq(rng, ref)

  var failed: Int = 0

  def run(): Unit = {
    var waitingRead: Option[BigInt] = None
    for (i <- (0 until len)) {
      // println("Cycle: " + i)
      dut.io.reader.req.valid.poke(if (waitingRead.isEmpty && cur.isRead) {
        true.B
      } else { false.B })
      // read port should be aligned
      dut.io.reader.req.bits.addr.poke(align(cur.addr).U)

      dut.io.writer.op.poke(
        if (cur.isRead) { DCWriteOp.idle }
        else { DCWriteOp.write }
      )
      dut.io.writer.addr.poke(cur.addr.U)
      dut.io.writer.len.poke(DCWriteLen.safe(cur.lg2Len.U)._1)
      dut.io.writer.wdata.poke(cur.wdata.U)

      if (cur.isRead && dut.io.reader.req.ready.peek.litToBoolean == true) {
        waitingRead match {
          case None => {
            waitingRead = Some(ref.get(align(cur.addr)).get)
          }
          case Some(v) => {
            if (dut.io.reader.resp.bits.peek.litValue != v) {
              if (failed < 10) {
                println(
                  s"[${i}] 0x${cur.addr.toHexString}, Expected 0x${v
                    .toString(16)}, got 0x${dut.io.reader.resp.bits.peek.litValue
                    .toString(16)}"
                )
                failed += 1;
              }
            }
            cur = nextReq(rng, ref)
            waitingRead = None
            cnt += 1
          }
        }
      } else if (
        !cur.isRead && dut.io.writer.stall.peek.litToBoolean == false
      ) {
        val aligned = align(cur.addr)
        val original = ref.get(aligned).getOrElse(BigInt(0))

        // compute DCWriter.be
        // 0x0, D => 0b11111111
        // 0x1, B => 0b00000010
        // 0x2, H => 0b00001100
        // 0x4, W => 0b11110000
        val offset = (cur.addr & 0x7).toInt
        val mask = BigInt(1 << (1 << cur.lg2Len)) - 1
        val be = mask << offset

        // convert byte mask to bit mask
        var wmask = BigInt(0)
        for (i <- (0 until 8)) {
          if (((be >> i) & 1) != 0) {
            wmask |= 0xff << (i * 8)
          }
        }

        val wdata = (cur.wdata << (offset * 8))
        val muxed =
          (wdata & wmask) | (original & ((BigInt(1) << 64) - 1 - wmask))

        ref.put(aligned, muxed)

        cur = nextReq(rng, ref)
        cnt += 1
      }

      dut.clock.step(1)
    }
  }

  run()
  println(s"Cycle count: ${len}, total ops: ${cnt}")
  assert(failed == 0)
}

object L2CacheTest {
  val RAM_SIZE = 65536
  val W_RATIO = 0.1
}

object L2CacheSpec {
  val DEFAULT_SEED = 0L
  val DEFAULT_LENGTH = 10000
}

class L2CacheSpec extends AnyFlatSpec with Matchers with ChiselScalatestTester {
  behavior of "L2CacheSpec"

  it should s"run successfully" in {
    val seed = L2CacheSpec.DEFAULT_SEED
    val len = L2CacheSpec.DEFAULT_LENGTH
    test(new WrappedL2(L1DCacheTestDef, L2CacheTestDef, CacheTestDef))
      .withAnnotations(Simulator.getAnnotations()) { dut =>
        new L2CacheTest(dut, seed, len)
      }
  }
}
