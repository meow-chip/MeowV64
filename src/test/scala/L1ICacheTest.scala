import core._
import util._
import cache._
import chisel3._
import chisel3.iotesters.PeekPokeTester
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.util.Random
import scala.collection.mutable.HashMap

object L1CacheTestDef extends {
  val ADDR_WIDTH: Int = 48
  val ASSOC: Int = 2
  val LINE_WIDTH: Int = 16
  val SIZE: Int = 2048 // 4K L1 I
  val TRANSFER_SIZE: Int = 2 * 16
  val XLEN: Int = 64
} with L1Opts;

class L1ICacheTest(dut: L1IC, seed: Long, len: Int) extends PeekPokeTester(dut) {
  println(s"Testing cache with seed: ${seed} for ${len} cycles")

  val rng = new Random(seed)

  var cnt = 0

  val addrs: Seq[Option[Int]] = Seq.fill(len)({
    val en = rng.nextBoolean()
    val addr = (rng.nextInt() % L1ICacheTest.RAM_SIZE >> 2) << 2

    if(en) {
      Some(addr)
    } else {
      None
    }
  })
  val ref = new HashMap[Int, Int]()

  // Populate ref
  for(addr <- addrs) {
    if(addr.isDefined && !ref.keySet.contains(addr.get)) {
      ref.put(addr.get, rng.nextInt())
    }
  }

  var failed: Int = 0;

  def run(): Unit = {
    for(i <- (0 until len)) {
      // println("Cycle: " + i)
      poke(dut.toCPU.read, addrs(cnt).isDefined)
      poke(dut.toCPU.addr, addrs(cnt).getOrElse(rng.nextInt() % L1ICacheTest.RAM_SIZE))

      if(peek(dut.toCPU.stall) == 0) {
        // Check last
        if(cnt == 0 || addrs(cnt-1).isEmpty) {
          if(!expect(dut.toCPU.vacant, 1)) {
            if(failed < 10) {
              println(s"[${i}] Expected vacant output on req ${cnt}")
              failed += 1;
            }
          }
        } else {
          if(!expect(dut.toCPU.vacant, 0)) {
            if(failed < 10) {
              println(s"[${i}] Expected non-vacant output on req ${cnt}")
              failed += 1;
            }
          }

          if(!expect(dut.toCPU.data, ref(addrs(cnt-1).get))) {
            if(failed < 10) {
              println(s"[${i}] 0x${addrs(cnt-1).get.toHexString}, Expected 0x${ref(addrs(cnt-1).get).toHexString}, got ${peek(dut.toCPU.data).toString(16)}")
              failed += 1;
            }
          }
        }

        cnt = cnt + 1;
      }

      var heldCycles: Option[Int] = None

      if(peek(dut.toL2.read) == 1) {
        // Randomily waits on L2

        heldCycles = heldCycles match {
          case None => Some(rng.nextInt(4))
          case Some(c) => Some(c-1)
        }

        if(heldCycles == Some(0)) {
          poke(dut.toL2.stall, false)
          poke(dut.toL2.data, ref(peek(dut.toL2.addr).toInt))
        } else {
          poke(dut.toL2.stall, true)
        }
      } else {
        poke(dut.toL2.stall, false)
      }


      step(1)
    }
  }

  run()
  println(s"Cycle count: ${len}, total ops: ${cnt}")
}

object L1ICacheTest {
  val RAM_SIZE = 65536
  val W_RATIO = 0.1

  def run(seed: Long, len: Int, args: Option[Array[String]] = None): Boolean = {
    args match {
      case None => chisel3.iotesters.Driver(
        () => new L1IC(L1CacheTestDef),
        "verilator"
      ) { new L1ICacheTest(_, seed, len) }

      case Some(args) => chisel3.iotesters.Driver.execute(
        args,
        () => new L1IC(L1CacheTestDef)
      ) { new L1ICacheTest(_, seed, len) }
    }
  }
}

object L1ICacheSpec {
  val DEFAULT_SEED = 0L
  val DEFAULT_LENGTH = 100000
}

class L1ICacheSpec extends FlatSpec with Matchers {
  behavior of "CacheSpec"

  it should s"run successfully" in { L1ICacheTest.run(L1ICacheSpec.DEFAULT_SEED, L1ICacheSpec.DEFAULT_LENGTH) should be(true) }
}

object L1ICacheTestMain extends App {
  var seed = L1ICacheSpec.DEFAULT_SEED
  var length = L1ICacheSpec.DEFAULT_LENGTH

  if(args.length > 2) {
    seed = java.lang.Long.parseLong(args(args.length-2))
    length = Integer.parseInt(args(args.length-1))
  } else if(args.length > 1) {
    seed = java.lang.Long.parseLong(args(args.length-1))
  }

  println(s"Running with seed ${seed} for ${length} cycles...")
  L1ICacheTest.run(seed, length, if(args.length > 0) { Some(args) } else { None })
}
