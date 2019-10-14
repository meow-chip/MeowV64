import chisel3._
import _root_.data._

class Core extends Module {
    val io = IO(new Bundle {
        val axi = new AXI(8)
    })
}