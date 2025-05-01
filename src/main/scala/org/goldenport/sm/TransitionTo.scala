package org.goldenport.sm

import org.goldenport.context._
import org.goldenport.sm.StateMachine.RuleAndState

/*
 * @since   May.  2, 2021
 *  version May. 29, 2021
 *  version Jun. 12, 2021
 * @version Sep.  5, 2024
 * @author  ASAMI, Tomoharu
 */
trait TransitionTo {
  def state(sm: StateMachine, state: State, p: Parcel): RuleAndState
}

object TransitionTo {
}

object NoneTransitionTo extends TransitionTo {
  def state(sm: StateMachine, state: State, p: Parcel) = RuleAndState(sm.currentStateMachineRule, state)
}

object FinalTransitionTo extends TransitionTo {
  def state(sm: StateMachine, state: State, p: Parcel) = RuleAndState(sm.currentStateMachineRule, State.finalState)
}

case class HistoryTransitionTo() extends TransitionTo {
  def state(sm: StateMachine, state: State, p: Parcel) = sm.historyState()
}

case class NameTransitionTo(name: String) extends TransitionTo {
  def state(sm: StateMachine, state: State, p: Parcel) =
    sm.getStateClass(name) match {
      case Some(s) => RuleAndState(s.rule, p.context.logic.newState(s.state))
      case None => IllegalConfigurationDefect(s"to: $name").RAISE
    }
}
