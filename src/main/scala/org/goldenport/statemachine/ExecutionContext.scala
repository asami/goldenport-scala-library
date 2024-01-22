package org.goldenport.statemachine

import scala.util.control.NonFatal
import org.goldenport.RAISE
import org.goldenport.trace.{TraceContext, Result}

/*
 * @since   May.  5, 2021
 *  version May. 30, 2021
 *  version Jun.  6, 2021
 *  version Nov. 29, 2021
 * @version Jan. 22, 2022
 * @author  ASAMI, Tomoharu
 */
class ExecutionContext(
  traceContext: TraceContext,
  logicOption: Option[StateMachineLogic] = None,
  val opaque: Option[AnyRef] = None
) {
  def logic = logicOption getOrElse RAISE.noReachDefect("ExecutionContext#logic: no StateMachineLogic")

  def withClass(p: StateMachineClass): ExecutionContext = 
    new ExecutionContext(traceContext, Some(p.logic), opaque)

//  def getStateClass(name: String): Option[StateClass] = logic.getStateClass(name)

  def execute[T](label: String, enter: Any)(body: => Result[T]): T = traceContext.execute(label, enter)(body)

  def execute(sm: StateMachine, activity: Activity, p: Parcel): Parcel = logic.execute(sm, activity, p)
}

object ExecutionContext {
  def create(): ExecutionContext = new ExecutionContext(TraceContext.create())
  def create(p: StateMachineLogic): ExecutionContext = new ExecutionContext(TraceContext.create(), Some(p))
  def create(trace: TraceContext, logic: StateMachineLogic, opaque: AnyRef): ExecutionContext =
    new ExecutionContext(trace, Some(logic), Some(opaque))

  def createPreparation(trace: TraceContext, opaque: AnyRef): ExecutionContext =
    new ExecutionContext(trace, None, Some(opaque))

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
