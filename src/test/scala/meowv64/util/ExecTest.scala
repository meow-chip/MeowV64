package meowv64.util

import meowv64.multicore.Multicore
import meowv64.multicore.MulticoreDef

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.file.Paths
import scala.collection.mutable

import chisel3._
import chisel3.tester._
import org.scalatest.freespec.AnyFreeSpec

object ExecDef extends MulticoreDef {
  override val INIT_VEC = BigInt(0x80000000L)
}

class ExecTest(dut: Multicore, file: String) {
  def doTest(bound: Int): Unit = {
    val mem: mutable.HashMap[Long, Long] = mutable.HashMap() // Addr to 4-byte
    var reading: Option[(Long, Long, Long, Long)] = None // ID, Ptr, Left, Size
    var writing: Option[(Long, Long, Long)] = None // Ptr, Left, Size
    var writingFinished = false

    // Load up memory
    val bytes = java.nio.file.Files.readAllBytes(Paths.get(file))
    val len = bytes.length
    val paddedLen = ((len + 7) / 8) * 8
    val padded = bytes ++ Array.fill(paddedLen - len)(0.asInstanceOf[Byte])
    val buffer = ByteBuffer.wrap(padded)
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    val longs = buffer.asLongBuffer()
    var idx = 0
    while (longs.hasRemaining()) {
      mem.put(idx * 8 + 0x80000000L, longs.get())
      idx += 1
    }
    println(s"Initialized: $idx longs")

    val axi = dut.io.axi
    val failed: mutable.HashSet[Long] = mutable.HashSet.empty

    // Test pass signal through tohost
    var finished = false
    for (i <- (0 until bound)) {
      // Always have interrupt 1 set at high
      dut.io.eints(1).poke(true.B)

      // println("Cycle: " + i)
      dut.clock.step(1)

      // TODO: handles multicore
      if (dut.io.debug(0).pc.peek.litValue == 0x100000 || finished) {
        println(s"> Process ended at cycle ${i}")

        val mcycle = dut.io.debug(0).mcycle.peek.litValue
        val minstret = dut.io.debug(0).minstret.peek.litValue

        println(s"> mcycle: ${mcycle}")
        println(s"> minstret: ${minstret}")

        return
      }

      // Simulate AXI
      // AR
      if (reading.isEmpty && axi.ARVALID.peek == 1) {
        axi.ARREADY.poke(true.B)
        reading = Some(
          (
            axi.ARID.peek.litValue.toLong,
            axi.ARADDR.peek.litValue.toLong,
            axi.ARLEN.peek.litValue.toLong,
            axi.ARSIZE.peek.litValue.toLong
          )
        )
        // println(s"Read: 0x${reading.get._2.toHexString}")
      } else {
        axi.ARREADY.poke(false.B)
      }

      // R
      if (reading.isDefined) {
        val (id, ptr, left, size) = reading.get
        axi.RVALID.poke(true.B)
        axi.RID.poke(id.U)
        val rdata = if (ptr == 0x10001014) { // LSR
          1L << (32 + 5)
        } else {
          mem.get((ptr >> 3) << 3).getOrElse(0L)
        }
        val mask = if (size == 3) {
          0xffffffffffffffffL
        } else {
          (1L << ((1L << size) * 8)) - 1L
        }
        val shiftedMask = mask << ((ptr & 7) * 8)
        // println(s"  Raw: 0x${rdata.toHexString}")
        // println(s"  Shifted: 0x${shifted.toHexString}")
        // println(s"  Mask: 0x${mask.toHexString}")
        axi.RDATA.poke((rdata & shiftedMask).U)
        axi.RLAST.poke((left == 0).B)
        // println(s"Returning: 0x${(shifted & mask).toHexString}")

        if (axi.RREADY.peek == 1) {
          if (left == 0) reading = None
          else reading = Some((id, ptr + (1 << size), left - 1, size))
        }
      } else {
        axi.RVALID.poke(false.B)
      }

      // AW
      if (writing.isEmpty && axi.AWVALID.peek == 1) {
        axi.AWREADY.poke(true.B)
        writing = Some(
          (
            axi.AWADDR.peek.litValue.toLong,
            axi.AWLEN.peek.litValue.toLong,
            axi.AWSIZE.peek.litValue.toLong
          )
        )
        writingFinished = false
        // println(s"Write: 0x${writing.get._1.toHexString}")
      } else {
        axi.AWREADY.poke(false.B)
      }

      // W
      if (writing.isDefined && !writingFinished) {
        val (ptr, left, size) = writing.get
        axi.WREADY.poke(true.B)
        val wdata = axi.WDATA.peek
        val wstrb = axi.WSTRB.peek

        if (axi.WVALID.peek == 1) {
          val muxed = ExecTest.longMux(
            mem.get((ptr >> 3) << 3).getOrElse(0),
            wdata.litValue.toLong,
            wstrb.litValue.toByte,
            ptr & 7,
            1L << size
          )
          mem.put((ptr >> 3) << 3, muxed)

          writing match {
            case Some((addr, _, _)) if addr == 0x10001000L => {
              // Print to serial
              print((wdata.litValue & 0xff).toChar)
            }
            case Some((addr, _, _)) if addr == 0x20000000L => {
              // tohost in ISA testsuite
              val data = (wdata.litValue & 0xffffffff).toLong
              if (wdata == ((data & 0xff) | BigInt("0101000000000000", 16))) {
                // Is simple print
                print((data & 0xff).toChar)
              } else if (data == 1) {
                println("ISA testsuite pass.")
                finished = true
              } else if ((data & 1) == 1) {
                val c = data >> 1
                if (!failed.contains(c)) {
                  println(s"ISA testsuit failed case ${c}")
                  failed.add(c)
                }
              } else {
                println(s"ISA testsuite tohost: ${data}")
              }
            }
            case _ => {}
          }

          writing = Some((ptr + (1 << size), left - 1, size))
          if (axi.WLAST.peek == 1) {
            assume(left == 0)
            writingFinished = true
          }
        }
      } else {
        axi.WREADY.poke(false.B)
      }

      if (writingFinished) {
        assume(writing.isDefined)
        axi.BVALID.poke(true.B)
        axi.BRESP.poke(0.U)
        axi.BID.poke(0.U)

        if (axi.BREADY.peek == 1) {
          writing = None
          writingFinished = false
        }
      } else {
        axi.BVALID.poke(false.B)
      }
    }

    throw new Error(s"Did not finished within ${bound} cycles")
  }

  doTest(100000)
}

object ExecTest extends AnyFreeSpec with ChiselScalatestTester  {
  def runFile(file: String, args: Option[Array[String]] = None): Boolean = {
    println(s"------------\nRunning file $file")
      test(
        new Multicore()(ExecDef),
      ) {
        new ExecTest(_, file)
      }
      return true
  }

  def longMux(
      base: Long,
      input: Long,
      be: Byte,
      offset: Long,
      size: Long
  ): Long = {
    var ret = 0L
    // println(s"Muxing: 0x${base.toHexString} <- 0x${input.toHexString} & 0x${be}, offset $offset, size $size")
    for (i <- (0 until 8)) {
      val sel = if (i < offset) {
        (base >>> (i * 8)) & 0xff
      } else if (i >= offset + size) {
        (base >>> (i * 8)) & 0xff
      } else if (((be >>> i) & 1) == 1) {
        (input >>> (i * 8)) & 0xff
      } else {
        (base >>> (i * 8)) & 0xff
      }
      ret = (sel << (i * 8)) | ret
    }

    ret
  }
}