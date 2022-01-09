/*
import core._
import util._
import cache._
import chisel3._
import chisel3.iotesters.PeekPokeTester
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.util.Random
import scala.collection.mutable.HashMap

object CacheTestDef extends CoreDef {}

object L1DCacheTestDef extends {
  val ADDR_WIDTH: Int = 48
  val ASSOC: Int = 2
  val LINE_BYTES: Int = 16
  val SIZE: Int = 128
  val TRANSFER_WIDTH: Int = 64
  val XLEN: Int = 64

  val WRITE_BUF_DEPTH = 2
} with L1DOpts;

object L2CacheTestDef extends {
  val ADDR_WIDTH: Int = 48
  val ASSOC: Int = 4
  val CORE_COUNT: Int = 1
  val LINE_BYTES: Int = 16
  val TRANSFER_WIDTH: Int = 0 // Actually ignored
  val SIZE: Int = 256
  val WB_DEPTH: Int = 4
  val XLEN: Int = 64
} with L2Opts

class WrappedL2(l1do: L1DOpts, l2o: L2Opts, implicit val coredef: CoreDef) extends Module {
  val io = IO(new Bundle {
    val reader = Flipped(new DCReader(l1do))
    val writer = Flipped(new DCWriter(l1do))
  })

  val mem = Module(new AXIMem(None, L2CacheTest.RAM_SIZE, 48))

  val l2 = Module(new L2Cache(l2o))
  val l1d = Module(new L1DC(l1do))

  l2.directs(0) <> L1UCPort.empty(l1do)
  l2.ic(0) <> L1ICPort.empty(l1do)
  l2.dc(0) <> l1d.toL2

  io.reader <> l1d.r
  io.writer <> l1d.w

  l2.axi <> mem.io.axi
}

class L2CacheTest(dut: WrappedL2, seed: Long, len: Int) extends PeekPokeTester(dut) {
  println(s"Testing cache with seed: ${seed} for ${len} cycles")

  val rng = new Random(seed)

  class Req(
    val isRead: Boolean,
    val addr: Int,
    val wdata: Long,
    val wbe: Int
  )

  def align(input: Long): Long = (input >> 3) << 3

  def genReq(rng: Random): Req = new Req(
    rng.nextFloat() > L2CacheTest.W_RATIO,
    align(Math.abs(rng.nextLong()) % L2CacheTest.RAM_SIZE),
    Math.abs(rng.nextLong()),
    Math.abs(rng.nextInt()) % 256 // 8-bit write-enable
  )

  var cnt = 0

  val ref = new HashMap[Int, Long]()

  def nextReq(rng: Random, ref: HashMap[Int, Long]): Req = {
    val ret = genReq(rng)

    val valid = ref.contains(ret.addr)

    if(valid) {
      return ret
    } else {
      return new Req(
        false,
        ret.addr,
        ret.wdata,
        0xFF // Write whole byte
      )
    }

    ret
  }

  var cur: Req = nextReq(rng, ref)

  var failed: Int = 0;

  def run(): Unit = {
    var waitingRead: Option[Long] = None
    for(i <- (0 until len)) {
      // println("Cycle: " + i)
      poke(dut.io.reader.read, if(waitingRead.isEmpty && cur.isRead) { 1 } else { 0 })
      poke(dut.io.reader.addr, cur.addr)

      poke(dut.io.writer.write, if(cur.isRead) { 0 } else { 1 })
      poke(dut.io.writer.addr, cur.addr)
      poke(dut.io.writer.data, cur.wdata)
      poke(dut.io.writer.be, cur.wbe)

      if(cur.isRead && peek(dut.io.reader.stall) == 0) {
        waitingRead match {
          case None => {
            waitingRead = Some(ref.get(cur.addr).get)
          }
          case Some(v) => {
            if(!expect(dut.io.reader.data, v)) {
              if(failed < 10) {
                println(s"[${i}] 0x${cur.addr.toHexString}, Expected 0x${v.toHexString}, got ${peek(dut.io.reader.data).toString(16)}")
                failed += 1;
              }
            }
            cur = nextReq(rng, ref)
            waitingRead = None
            cnt += 1
          }
        }
      } else if(!cur.isRead && peek(dut.io.writer.stall) == 0) {
        var original = ref.get(cur.addr).getOrElse(0L)

        var wmask: Long = 0

        for(i <- (0 until 8)) {
          if(((cur.wbe >> i) & 1) != 0) {
            wmask |= 0xFFL << (i*8)
          }
        }

        val muxed = (cur.wdata & wmask) | (original & (~wmask))

        ref.put(cur.addr, muxed)

        cur = nextReq(rng, ref)
        cnt += 1
      }

      step(1)
    }
  }

  run()
  println(s"Cycle count: ${len}, total ops: ${cnt}")
}

object L2CacheTest {
  val RAM_SIZE = 65536
  val W_RATIO = 0.1

  def run(seed: Long, len: Int, args: Option[Array[String]] = None): Boolean = {
    args match {
      case None => chisel3.iotesters.Driver(
        () => new WrappedL2(L1DCacheTestDef, L2CacheTestDef),
        "verilator"
      ) { new L2CacheTest(_, seed, len) }

      case Some(args) => chisel3.iotesters.Driver.execute(
        args,
        () => new WrappedL2(L1DCacheTestDef, L2CacheTestDef)
      ) { new L2CacheTest(_, seed, len) }
    }
  }
}

object L2CacheSpec {
  val DEFAULT_SEED = 0L
  val DEFAULT_LENGTH = 100000
}

class L2CacheSpec extends FlatSpec with Matchers {
  behavior of "CacheSpec"

  it should s"run successfully" in { L2CacheTest.run(L2CacheSpec.DEFAULT_SEED, L2CacheSpec.DEFAULT_LENGTH) should be(true) }
}

object L2CacheTestMain extends App {
  var seed = L2CacheSpec.DEFAULT_SEED
  var length = L2CacheSpec.DEFAULT_LENGTH

  if(args.length > 2) {
    seed = java.lang.Long.parseLong(args(args.length-2))
    length = Integer.parseInt(args(args.length-1))
  } else if(args.length > 1) {
    seed = java.lang.Long.parseLong(args(args.length-1))
  }

  println(s"Running with seed ${seed} for ${length} cycles...")
  L2CacheTest.run(seed, length, if(args.length > 0) { Some(args) } else { None })
}
 */
