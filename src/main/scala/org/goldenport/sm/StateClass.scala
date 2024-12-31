package org.goldenport.sm

import org.goldenport.context.Consequence
import org.goldenport.values.PathName

/*
 * @since   Jan.  4, 2021
 *  version May. 29, 2021
 *  version Jun. 29, 2021
 *  version Oct. 24, 2021
 *  version Nov. 28, 2021
 *  version Aug. 22, 2022
 *  version Sep.  3, 2022
 * @version Sep.  5, 2024
 * @author  ASAMI, Tomoharu
 */
case class StateClass(
  name: String,
  value: Int,
  stateMachinePath: Option[PathName] = None,
  transitions: Transitions = Transitions.empty,
  entryActivity: Activity = Activity.Empty,
  exitActivity: Activity = Activity.Empty,
  doActivity: DoActivity = DoActivity.Empty
) {
  def qualifiedFullName: String = stateMachinePath.fold(name)(x => (x :+ name).v)

  def withTransitions(p: Transitions) = copy(transitions = p)
  def withTransitions(p: Transition, ps: Transition*) = copy(transitions = Transitions.global(p +: ps))
  def withEntryActivity(p: Activity) = copy(entryActivity = p)
  def withExitActivity(p: Activity) = copy(exitActivity = p)
  def withDoActivity(p: DoActivity) = copy(doActivity = p)

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

  def checkChageState(
    sm: StateMachine,
    state: State,
    p: Int
  ): Boolean = transitions.checkChageState(sm, state, p)
}

object StateClass {
  val initState = StateClass(STATE_NAME_INIT, STATE_VALUE_INIT)
  val finalState = StateClass(STATE_NAME_FINAL, STATE_VALUE_FINAL)
  val canceled = StateClass(STATE_NAME_CANCELED, STATE_VALUE_CANCELED)
  val suspended = StateClass(STATE_NAME_SUSPENDED, STATE_VALUE_SUSPENDED)
  val running = StateClass(STATE_NAME_RUNNING, STATE_VALUE_RUNNING)
  val confirming = StateClass(STATE_NAME_CONFIRMING, STATE_VALUE_CONFIRMING)
  val confirmed = StateClass(STATE_NAME_CONFIRMED, STATE_VALUE_CONFIRMED)
  val rejected = StateClass(STATE_NAME_REJECTED, STATE_VALUE_REJECTED)
  val delivering = StateClass(STATE_NAME_DELIVERING, STATE_VALUE_DELIVERING)
  val delivered = StateClass(STATE_NAME_DELIVERED, STATE_VALUE_DELIVERED)

  val predefinedStates = List(
    initState,
    finalState,
    canceled,
    suspended,
    running,
    confirming,
    confirmed,
    rejected,
    delivering,
    delivered
  )

  // case class StateValue(name: String, value: Int)

  // val defaultStateValues = Vector(
  //   StateValue(STATE_NAME_INIT, STATE_VALUE_INIT),
  //   StateValue(STATE_NAME_FINAL, STATE_VALUE_FINAL),
  //   StateValue(STATE_NAME_CANCELED, STATE_VALUE_CANCELED),
  //   StateValue(STATE_NAME_SUSPENDED, STATE_VALUE_SUSPENDED),
  //   StateValue(STATE_NAME_RUNNING, STATE_VALUE_RUNNING),
  //   StateValue(STATE_NAME_CONFIRMING, STATE_VALUE_CONFIRMING),
  //   StateValue(STATE_NAME_CONFIRMED, STATE_VALUE_CONFIRMED),
  //   StateValue(STATE_NAME_REJECTED, STATE_VALUE_REJECTED),
  //   StateValue(STATE_NAME_DELIVERING, STATE_VALUE_DELIVERING),
  //   StateValue(STATE_NAME_DELIVERED, STATE_VALUE_DELIVERED)
  // )

  val predefinedStateValues = predefinedStates.map(x => x.name -> x.value).toMap

  def apply(name: String, value: Int, transitions: Transitions): StateClass =
    StateClass(name, value, None, transitions)

  def create(name: String): StateClass = apply(name, predefinedStateValues(name))

  def create(name: String, transitions: Transitions): StateClass = 
    apply(name, predefinedStateValues(name), transitions)
}
