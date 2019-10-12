import chisel3._
import data._

class Core extends Module {
    val io = IO(new Bundle {
        val axi = new AXI()
    })
}