package org.goldenport.statemachine

import org.goldenport.context.Consequence

/*
 * @since   Jan.  4, 2021
 * @version May. 29, 2021
 * @author  ASAMI, Tomoharu
 */
case class State(
  clazz: StateClass
  // major: MajorState,
  // minor: MinorState
) {
  def status: String = clazz.name
  def entryActivity: Activity = clazz.entryActivity
  def exitActivity: Activity = clazz.exitActivity
  def doActivity: DoActivity = clazz.doActivity

  def accept(p: Parcel): Consequence[Boolean] = clazz.accept(this, p)
  def transit(p: Parcel): Consequence[(State, Transition, Parcel)] = clazz.transit(this, p)
}

object State {
  val finalState = State(StateClass.finalState)
}
