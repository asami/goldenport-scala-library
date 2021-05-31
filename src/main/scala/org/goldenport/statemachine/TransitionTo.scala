package org.goldenport.statemachine

import org.goldenport.context._

/*
 * @since   May.  2, 2021
 * @version May. 29, 2021
 * @author  ASAMI, Tomoharu
 */
trait TransitionTo {
  def state(state: State, p: Parcel): State
}

object TransitionTo {
}

object NoneTransitionTo extends TransitionTo {
  def state(state: State, p: Parcel): State = state
}

object FinalTransitionTo extends TransitionTo {
  def state(state: State, p: Parcel): State = State.finalState
}

case class NameTransitionTo(name: String) extends TransitionTo {
  def state(state: State, p: Parcel): State = {
    p.context.getStateClass(name).
      map(p.context.logic.newState).
      getOrElse(IllegalConfigurationDefect(s"to: $name").RAISE)
  }
}
