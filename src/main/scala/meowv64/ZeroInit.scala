package meowv64

import firrtl.CircuitState
import firrtl.DependencyAPIMigration
import firrtl.Transform
import firrtl.ir.ExtModule
import firrtl.ir._

/** Zero-initialized registers if no reset is specified.
  */
object ZeroInit extends Transform with DependencyAPIMigration {
  override def prerequisites = Seq()

  override def invalidates(a: Transform) = false

  override protected def execute(state: CircuitState): CircuitState = {
    state.copy(circuit = state.circuit.mapModule(onModule(_)))
  }

  private def onModule(m: DefModule): DefModule =
    m match {
      case e: ExtModule => e
      case mod: Module =>
        mod.copy(body = mod.body.mapStmt(onStmt))
    }

  private def onStmt(s: Statement): Statement = s match {
    // special handle for RRArbiter lastGrant
    // see https://github.com/chipsalliance/chisel3/pull/2329
    case i: DefRegister => {
      if (i.name == "lastGrant" && i.tpe == UIntType(IntWidth(1))) {
        println(s"Hacking lastGrant")
        i.copy(reset = Reference("reset"), init = UIntLiteral(0, IntWidth(1)))
      } else {
        i
      }
    }
    case other => other.mapStmt(onStmt(_))
  }
}
