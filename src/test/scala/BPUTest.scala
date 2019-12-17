import instr.BPU
import core.CoreDef
import chisel3._
import chisel3.iotesters.{PeekPokeTester, Driver}
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.util.Random
import scala.collection.mutable.HashMap


class BPUTest(bpu: BPU) extends PeekPokeTester(bpu) {
    // smoke test case 1:
    // upd: pc = 0x420, taken = true
    // upd: pc = 0x430, taken = false
    // query: pc = 0x420
    // query: pc = 0x430
    
    poke(bpu.toFetch.query, false)
    poke(bpu.toCtrl.upd, true)
    poke(bpu.toCtrl.actpc, 0x420)
    poke(bpu.toCtrl.fired, true)
    step(1)

    poke(bpu.toFetch.query, false)
    poke(bpu.toCtrl.upd, true)
    poke(bpu.toCtrl.actpc, 0x430)
    poke(bpu.toCtrl.fired, false)
    step(1)

    poke(bpu.toFetch.query, false)
    poke(bpu.toFetch.pc, 0x420)
    poke(bpu.toCtrl.upd, false)
    step(1)
    assert(peek(bpu.toFetch.taken) == BigInt(1))

    poke(bpu.toFetch.query, false)
    poke(bpu.toFetch.pc, 0x430)
    poke(bpu.toCtrl.upd, false)
    step(1)
    assert(peek(bpu.toFetch.taken) == BigInt(0))
}

object DefaultDef extends CoreDef

object BPUTestMain extends App {
    if (!Driver(() => new BPU(DefaultDef, 16, 1))(c => new BPUTest(c))) System.exit(1)
}