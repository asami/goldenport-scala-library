package org.goldenport.statemachine

import org.goldenport.context.Consequence
import org.goldenport.values.PathName

/*
 * @since   Jan.  4, 2021
 *  version May. 29, 2021
 *  version Jun. 29, 2021
 * @version Oct. 24, 2021
 * @author  ASAMI, Tomoharu
 */
case class StateClass(
  name: String,
  value: Int,
  stateMachinePath: Option[PathName],
  transitions: Transitions,
  entryActivity: Activity = NoneActivity,
  exitActivity: Activity = NoneActivity,
  doActivity: DoActivity = NoneDoActivity
) {
  def qualifiedFullName: String = stateMachinePath.fold(name)(x => (x :+ name).v)

  def accept(state: State, p: Parcel): Consequence[Boolean] = transitions.accept(p)

  def transit(sm: StateMachine, state: State, p: Parcel): Consequence[Option[(State, Transition, Parcel)]] =
    transitions.transit(sm, state, p)

  // def transit(sm: StateMachine, state: State, p: Parcel): Consequence[Option[(State, Transition, Parcel)]] =
  //   transitions.find(_.guard.accept(p)).map(t =>
  //     Consequence {
  //       val s = p.context.logic.transit(sm, state, t, p)
  //       (s.state, t, p)
  //     }
  //   )
}

object StateClass {
  val finalState = StateClass(PROP_STATE_FINAL, STATE_VALUE_FINAL, None, Transitions.empty)
}
