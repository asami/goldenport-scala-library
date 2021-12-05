package org.goldenport.statemachine

import org.goldenport.trace.Result
import org.goldenport.event._

/*
 * @since   May.  2, 2021
 *  version May. 30, 2021
 *  version Sep. 26, 2021
 * @version Nov. 28, 2021
 * @author  ASAMI, Tomoharu
 */
case class Parcel( // TODO Intension
  context: ExecutionContext,
  event: Event
) {
  def withClass(p: StateMachineClass): Parcel = copy(context = context.withClass(p))

  def execute[T](label: String, enter: Any)(body: => Result[T]): T = context.execute(label, enter)(body)

  def execute(sm: StateMachine, activity: Activity): Parcel = context.execute(sm, activity, this)
}
