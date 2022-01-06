package meowv64

import meowv64.cache._
import chisel3._
import chisel3.tester._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.HashMap
import scala.util.Random
import chiseltest.simulator.WriteVcdAnnotation

object L1CacheTestDef
    extends {
      val ADDR_WIDTH: Int = 48
      val ASSOC: Int = 2
      val LINE_WIDTH: Int = 16
      val SIZE: Int = 128 // 4K L1 I
      val TRANSFER_SIZE: Int = 2 * 16
      val XLEN: Int = 64
    }
    with L1Opts;

class L1ICacheTest(dut: L1IC, seed: Long, len: Int) {
  println(s"Testing cache with seed: ${seed} for ${len} cycles")

  val rng = new Random(seed)

  var cnt = 0

  val addrs: Seq[Option[Int]] = Seq.fill(len)({
    val en = rng.nextBoolean()
    val addr = ((math.abs(rng.nextInt()) % L1ICacheTest.RAM_SIZE) >> 2) << 2

    if (en) {
      Some(addr)
    } else {
      None
    }
  })
  val ref = new HashMap[Int, BigInt]()

  // Populate ref
  for (addr <- addrs) {
    if (addr.isDefined && !ref.keySet.contains(addr.get)) {
      ref.put(addr.get, BigInt(math.abs(rng.nextLong())) & 0xffff)
    }
  }

  var failed: Int = 0;

  def run(): Unit = {
    for (i <- (0 until len)) {
      // println("Cycle: " + i)
      dut.toCPU.read.poke(addrs(cnt).isDefined.B)
      dut.toCPU.addr.poke(
        (addrs(cnt).getOrElse(rng.nextInt().abs % L1ICacheTest.RAM_SIZE)).U
      )

      if (dut.toCPU.stall.peek.litToBoolean == false) {
        // Check last
        if (cnt == 0 || addrs(cnt - 1).isEmpty) {
          if (dut.toCPU.vacant.peek.litToBoolean != true) {
            if (failed < 10) {
              println(s"[${i}] Expected vacant output on req ${cnt}")
              failed += 1;
            }
          }
        } else {
          if (dut.toCPU.vacant.peek.litToBoolean != false) {
            if (failed < 10) {
              println(s"[${i}] Expected non-vacant output on req ${cnt}")
              failed += 1;
            }
          }

          if (dut.toCPU.data.peek.litValue != ref(addrs(cnt - 1).get)) {
            if (failed < 10) {
              println(
                s"[${i}] ${cnt - 1}: 0x${addrs(cnt - 1).get.toHexString}, Expected 0x${ref(addrs(cnt - 1).get)
                  .toString(16)}, got 0x${dut.toCPU.data.peek.litValue.toString(16)}"
              )
              failed += 1;
            }
          }
        }

        cnt = cnt + 1;
      }

      var heldCycles: Option[Int] = None

      if (dut.toL2.read.peek.litToBoolean == true) {
        // Randomly waits on L2

        heldCycles = heldCycles match {
          case None    => Some(rng.nextInt(4))
          case Some(c) => Some(c - 1)
        }

        if (heldCycles == Some(0)) {
          dut.toL2.stall.poke(false.B)

          val addr = dut.toL2.addr.peek.litValue.toInt
          var data = BigInt(0)
          for (i <- (0 until 4)) {
            val added = addr + i * 4;
            val v = ref.get(added).getOrElse(BigInt(0))
            data = data | (v << (i * 32))
          }

          dut.toL2.data.poke(data.U)
        } else {
          dut.toL2.stall.poke(true.B)
        }
      } else {
        dut.toL2.stall.poke(false.B)
      }

      dut.clock.step(1)
    }
  }

  run()
  println(s"Cycle count: ${len}, total ops: ${cnt}")
}

object L1ICacheTest {
  val RAM_SIZE = 65536
  val W_RATIO = 0.1
}

object L1ICacheSpec {
  val DEFAULT_SEED = 0L
  val DEFAULT_LENGTH = 1000
}

class L1ICacheSpec
    extends AnyFlatSpec
    with Matchers
    with ChiselScalatestTester {
  behavior of "CacheSpec"

  it should s"run successfully" in {
    val seed = L1ICacheSpec.DEFAULT_SEED
    val len = L1ICacheSpec.DEFAULT_LENGTH
    test(new L1IC(L1CacheTestDef)).withAnnotations(
      Seq(
        WriteVcdAnnotation
      )
    ) { dut =>
      new L1ICacheTest(dut, seed, len)
    }
  }
}