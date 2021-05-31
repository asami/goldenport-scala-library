package org.goldenport.statemachine

import org.goldenport.context.Consequence

/*
 * @since   Jan.  4, 2021
 * @version May. 29, 2021
 * @author  ASAMI, Tomoharu
 */
case class StateClass(
  name: String,
  transitions: Vector[Transition],
  entryActivity: Activity = NoneActivity,
  exitActivity: Activity = NoneActivity,
  doActivity: DoActivity = NoneDoActivity
) {
  def accept(state: State, p: Parcel): Consequence[Boolean] =
    Consequence(transitions.exists(_.guard.accept(p)))

  def transit(state: State, p: Parcel): Consequence[(State, Transition, Parcel)] =
    transitions.find(_.guard.accept(p)) match {
      case Some(t) => Consequence {
        val s = p.context.logic.transit(state, t, p)
        (s, t, p)
      }
      case None => Consequence.illegalConfigurationDefect(name)
    }
}

object StateClass {
  val finalState = StateClass(PROP_STATE_FINAL, Vector.empty)
}
