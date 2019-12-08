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

class WrappedL2(coredef: CoreDef) extends Module {
  val io = IO(new Bundle {
    val reader = Flipped(new DCReader(coredef.L1D))
    val writer = Flipped(new DCWriter(coredef.L1D))
  })

  val mem = Module(new AXIMem(None, CacheTest.RAM_SIZE, coredef.ADDR_WIDTH))

  val l2 = Module(new L2Cache(coredef.L2))
  val l1d = Module(new L1DC(coredef.L1D))

  l2.directs(0) <> L1UCPort.empty(coredef.L1D)
  l2.ic(0) <> L1ICPort.empty(coredef.L1I)
  l2.dc(0) <> l1d.toL2

  io.reader <> l1d.r
  io.writer <> l1d.w

  l2.axi <> mem.io.axi
}

class CacheTest(dut: WrappedL2, seed: Long, len: Int) extends PeekPokeTester(dut) {
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
    rng.nextFloat() > CacheTest.W_RATIO,
    align(Math.abs(rng.nextLong()) % CacheTest.RAM_SIZE),
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

object CacheTest {
  val RAM_SIZE = 65536
  val W_RATIO = 0.1

  def run(seed: Long, len: Int, args: Option[Array[String]] = None): Boolean = {
    args match {
      case None => chisel3.iotesters.Driver(
        () => new WrappedL2(CacheTestDef),
        "verilator"
      ) { new CacheTest(_, seed, len) }

      case Some(args) => chisel3.iotesters.Driver.execute(
        args,
        () => new WrappedL2(CacheTestDef)
      ) { new CacheTest(_, seed, len) }
    }
  }
}

object CacheSpec {
  val DEFAULT_SEED = 0L
  val DEFAULT_LENGTH = 100000
}

class CacheSpec extends FlatSpec with Matchers {
  behavior of "CacheSpec"

  it should s"run successfully" in { CacheTest.run(CacheSpec.DEFAULT_SEED, CacheSpec.DEFAULT_LENGTH) should be(true) }
}

object CacheTestMain extends App {
  var seed = CacheSpec.DEFAULT_SEED
  var length = CacheSpec.DEFAULT_LENGTH

  if(args.length > 2) {
    seed = java.lang.Long.parseLong(args(args.length-2))
    length = Integer.parseInt(args(args.length-1))
  } else if(args.length > 1) {
    seed = java.lang.Long.parseLong(args(args.length-1))
  }

  println(s"Running with seed ${seed} for ${length} cycles...")
  CacheTest.run(seed, length, Some(args))
}
