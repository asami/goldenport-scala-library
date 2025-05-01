package org.goldenport.sm

import org.goldenport.context.Consequence

/*
 * @since   Jan.  4, 2021
 *  version May. 25, 2021
 *  version Jun. 11, 2021
 *  version Jul.  9, 2021
 *  version Sep. 26, 2021
 *  version Nov. 28, 2021
 *  version Aug. 22, 2022
 *  version Sep.  3, 2022
 * @version Sep.  5, 2024
 * @author  ASAMI, Tomoharu
 */
case class Transition(
  guard: SmGuard,
  to: TransitionTo,
  effect: Activity
) {
  def getEventName: Option[String] = guard match {
    case EventNameGuard(name) => Some(name)
    case _ => None // TODO
  }
}

object Transition {
  def to(name: String, value: Int) = Transition(
    ToStateGuard(name, value),
    NameTransitionTo(name),
    Activity.Empty
  )
}
