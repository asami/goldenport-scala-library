package org.goldenport.statemachine

import org.goldenport.context._

/*
 * @since   May.  2, 2021
 * @version May. 27, 2021
 * @author  ASAMI, Tomoharu
 */
trait TransitionTo {
  def state(p: Parcel): State
}

object TransitionTo {
}

object NoneTransitionTo extends TransitionTo {
  def state(p: Parcel): State = ???
}

case class NameTransitionTo(name: String) extends TransitionTo {
  def state(p: Parcel): State = {
    p.context.getStateClass(name).
      map(p.context.logic.newState).
      getOrElse(IllegalConfigurationDefect(s"to: $name").RAISE)
  }
}
