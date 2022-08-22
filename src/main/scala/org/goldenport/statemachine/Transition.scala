package org.goldenport.statemachine

import org.goldenport.context.Consequence
import org.goldenport.event.CallEvent

/*
 * @since   Jan.  4, 2021
 *  version May. 25, 2021
 *  version Jun. 11, 2021
 *  version Jul.  9, 2021
 *  version Sep. 26, 2021
 *  version Nov. 28, 2021
 * @version Aug. 22, 2022
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

case class Transitions(
  call: Vector[Transition] = Vector.empty,
  global: Vector[Transition] = Vector.empty
) {
  def accept(p: Parcel): Consequence[Boolean] = Consequence(
    if (p.event.isInstanceOf[CallEvent])
      call.exists(_.guard.accept(p)) || global.exists(_.guard.accept(p))
    else
      global.exists(_.guard.accept(p)))

  def transit(sm: StateMachine, state: State, p: Parcel): Consequence[Option[(State, Transition, Parcel)]] =
    Consequence(
      if (p.event.isInstanceOf[CallEvent])
        _transit(sm, call, state, p) orElse _transit(sm, global, state, p)
      else
        _transit(sm, global, state, p)
    )

  private def _transit(sm: StateMachine, ts: Vector[Transition], state: State, p: Parcel): Option[(State, Transition, Parcel)] =
    ts.find(_.guard.accept(p)).map(_transit(sm, _, state, p))

  private def _transit(sm: StateMachine, t: Transition, state: State, p: Parcel): (State, Transition, Parcel) = {
    val s = p.context.logic.transit(sm, state, t, p)
    (s.state, t, p)
  }

  def findGoAhead(p: Parcel): Option[Transition] =
    call.find(_.guard.isGoAhead) orElse global.find(_.guard.isGoAhead)
}
object Transitions {
  val empty = Transitions(Vector.empty, Vector.empty)

  def global(ps: Seq[Transition]) = Transitions(Vector.empty, ps.toVector)
  def call(ps: Seq[Transition]) = Transitions(ps.toVector, Vector.empty)
}
