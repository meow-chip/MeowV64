package util

import chisel3.util.experimental.loadMemoryFromFile
import chisel3._
import chisel3.util._
import data.AXI
import firrtl.annotations.MemoryLoadFileType

class AXIMem(
  init: Option[String],
  depth: Int = 65536,
  addrWidth: Int = 48,
  dataWidth: Int = 64,
  serialAddr: Option[BigInt] = None
) extends Module {
  val io = IO(new Bundle {
    val axi = Flipped(new AXI(dataWidth, addrWidth))
  });

  // Async read, sync write mem
  val memory = Mem(depth, UInt(8.W))
  if(init.isDefined)
    loadMemoryFromFile(memory, init.get)
  
  // default signals
  io.axi <> DontCare
  io.axi.ARREADY := false.B
  io.axi.AWREADY := false.B
  io.axi.WREADY := false.B
  io.axi.RVALID := false.B
  io.axi.BVALID := false.B

  val sIDLE :: sREADING :: sWRITING :: sRESP :: sSERIAL_PRINT :: nil = Enum(5)

  val target = RegInit(0.U(addrWidth.W))
  val remaining: UInt = RegInit(0.U(log2Ceil(addrWidth).W))
  val state = RegInit(sIDLE)

  /*
  printf("Mem state:\n================\n")
  printf(p"State: ${state}\n\n")
  */

  // Assumes Burst = INCR
  switch(state) {
    is(sIDLE) {
      when(io.axi.ARVALID) {
        target := io.axi.ARADDR
        remaining := io.axi.ARLEN
        io.axi.ARREADY := true.B
        state := sREADING
      }

      when(io.axi.AWVALID) {
        target := io.axi.AWADDR
        remaining := io.axi.AWLEN
        io.axi.AWREADY := true.B
        state := sWRITING

        if(serialAddr.isDefined) {
          val sa: BigInt = serialAddr.get
          when(io.axi.AWADDR === sa.U) {
            // Override
            state := sSERIAL_PRINT
          }
        }
      }
    }

    is(sREADING) {
      // TODO: make 3 configurable
      val output = for(i <- 0 until dataWidth/8) yield memory(target(addrWidth-1, 3) ## i.U(3.W))
      io.axi.RDATA := Vec(output).asUInt
      io.axi.RVALID := true.B
      io.axi.RLAST := remaining === 1.U

      when(io.axi.RREADY) {
        /*
        printf(p"AXIMEM READING:\n")
        printf(p"  ADDR: 0x${Hexadecimal(RegNext(target))}\n")
        printf(p"  DATA: 0x${Hexadecimal(io.axi.RDATA)}\n")
        */

        target := target + 8.U
        remaining := remaining - 1.U
        when(remaining === 1.U) {
          state := sIDLE
        }
      }
    }

    is(sWRITING) {
      io.axi.WREADY := true.B
      when(io.axi.WVALID) {
        /*
        printf(p"AXIMEM WRITING:\n")
        printf(p"  ADDR: 0x${Hexadecimal(target)}\n")
        printf(p"  DATA: 0x${Hexadecimal(io.axi.WDATA)}\n")
        printf(p"  STRB: ${Hexadecimal(io.axi.WSTRB)}\n")
        */

        for(i <- 0 until (dataWidth/8)) {
          when(io.axi.WSTRB(i)) {
            memory.write(
              target(addrWidth-1, 3) ## i.U(3.W),
              io.axi.WDATA(i*8+7, i*8)
            )
          }
        }

        target := target + (dataWidth/8).U

        when(io.axi.WLAST) {
          state := sRESP
        }
      }
    }

    is(sRESP) {
      io.axi.BRESP := AXI.Constants.Resp.OKAY.U
      io.axi.BVALID := true.B

      when(io.axi.BREADY) {
        state := sIDLE
      }
    }

    is(sSERIAL_PRINT) {
      io.axi.WREADY := true.B

      when(io.axi.WVALID) {
        when(io.axi.WSTRB(0)) {
          printf(Character(io.axi.WDATA(7, 0)))
        }

        when(io.axi.WLAST) {
          state := sRESP
        }
      }
    }
  }
}
