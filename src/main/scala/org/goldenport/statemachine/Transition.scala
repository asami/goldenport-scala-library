package org.goldenport.statemachine

import org.goldenport.context.Consequence

/*
 * @since   Jan.  4, 2021
 *  version May. 25, 2021
 *  version Jun. 11, 2021
 * @version Jul.  9, 2021
 * @author  ASAMI, Tomoharu
 */
case class Transition(
  guard: SmGuard,
  activity: Activity,
  to: TransitionTo
) {
  def getEventName: Option[String] = guard match {
    case EventNameGuard(name) => Some(name)
    case _ => None // TODO
  }
}

case class Transitions(transitions: Vector[Transition]) {
  def accept(p: Parcel): Consequence[Boolean] = Consequence(transitions.exists(_.guard.accept(p)))

  def transit(sm: StateMachine, state: State, p: Parcel): Consequence[Option[(State, Transition, Parcel)]] =
    Consequence(
      transitions.find(_.guard.accept(p)).map { t =>
        val s = p.context.logic.transit(sm, state, t, p)
        (s.state, t, p)
      }
    )
}
object Transitions {
  val empty = Transitions(Vector.empty)
}
