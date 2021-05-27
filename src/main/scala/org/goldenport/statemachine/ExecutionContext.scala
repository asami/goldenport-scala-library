package org.goldenport.statemachine

import scala.util.control.NonFatal
import org.goldenport.RAISE

/*
 * @since   May.  5, 2021
 * @version May. 27, 2021
 * @author  ASAMI, Tomoharu
 */
class ExecutionContext(
  logicOption: Option[StateMachineLogic] = None
) {
  lazy val logic = logicOption getOrElse RAISE.noReachDefect

  // def states = statesOption getOrElse ???

  def getStateClass(name: String): Option[StateClass] = logic.getStateClass(name)
}

object ExecutionContext {
  def apply(): ExecutionContext = new ExecutionContext()
  def apply(p: StateMachineLogic): ExecutionContext = new ExecutionContext(Some(p))
  // private def apply(p: StateMachineRule): ExecutionContext = apply(p.states)
  // private def apply(p: List[StateClass]): ExecutionContext = ??? // new ExecutionContext(Some(p))

  // private def execute[T](body: ExecutionContext => T): T = execute(ExecutionContext())(body)

  // private def execute[T](rule: StateMachineRule)(body: ExecutionContext => T): T =
  //   execute(rule.states)(body)

  // private def execute[T](states: List[StateClass])(body: ExecutionContext => T): T =
  //   execute(ExecutionContext(states))(body)

  // private def execute[T](ctx: ExecutionContext)(body: ExecutionContext => T): T = try {
  //   body(ctx)
  // } catch {
  //   case NonFatal(e) => ???
  // }
}
