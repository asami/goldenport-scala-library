package org.goldenport.statemachine

import org.goldenport.context.Consequence
import org.goldenport.event.CallEvent

/*
 * @since   Sep.  3, 2022
 * @version Sep.  3, 2022
 * @author  ASAMI, Tomoharu
 */
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

  def checkChageState(sm: StateMachine, state: State, p: Int): Boolean = call.exists(_check_change_state(sm, state, p)) || global.exists(_check_change_state(sm, state, p))

  private def _check_change_state(sm: StateMachine, state: State, p: Int)(t: Transition) =
    t.to match {
      case NameTransitionTo(name) => sm.getStateClass(name).fold(false)(_.state.value == p)
      case _ => false
    }
}

object Transitions {
  val empty = Transitions(Vector.empty, Vector.empty)

  def global(ps: Seq[Transition]) = Transitions(Vector.empty, ps.toVector)
  def call(ps: Seq[Transition]) = Transitions(ps.toVector, Vector.empty)

  def apply(p: Transition, ps: Transition*): Transitions = apply(p +: ps)
  def apply(ps: Seq[Transition]): Transitions = call(ps)
}
