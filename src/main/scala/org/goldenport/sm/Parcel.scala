package org.goldenport.sm

import org.goldenport.trace.Result
import org.goldenport.event._

/*
 * @since   May.  2, 2021
 *  version May. 30, 2021
 *  version Sep. 26, 2021
 *  version Nov. 28, 2021
 *  version Aug. 11, 2022
 * @version Sep.  5, 2024
 * @author  ASAMI, Tomoharu
 */
case class Parcel( // TODO Intension
  context: ExecutionContext,
  event: Event,
  properties: Map[String, Any] = Map.empty
) {
  def withClass(p: StateMachineClass): Parcel = copy(context = context.withClass(p))

  def execute[T](label: String, enter: Any)(body: => Result[T]): T = context.execute(label, enter)(body)

  def execute(sm: StateMachine, activity: Activity): Parcel = context.execute(sm, activity, this)

  def get[T](key: String): Option[T] = properties.get(key).map(_.asInstanceOf[T])

  def set(key: String, value: Any): Parcel = copy(properties = properties + (key -> value))
}

object Parcel {

}
