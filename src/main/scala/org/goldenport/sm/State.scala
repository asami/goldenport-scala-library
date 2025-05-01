package org.goldenport.sm

import org.goldenport.context.Consequence

/*
 * @since   Jan.  4, 2021
 *  version May. 29, 2021
 *  version Jun. 29, 2021
 *  version Oct. 24, 2021
 *  version Sep.  2, 2022
 * @version Sep.  5, 2024
 * @author  ASAMI, Tomoharu
 */
case class State(
  clazz: StateClass
  // major: MajorState,
  // minor: MinorState
) {
  def status: String = clazz.name
  def value: Int = clazz.value
  def qualifiedFullName: String = clazz.qualifiedFullName
  def entryActivity: Activity = clazz.entryActivity
  def exitActivity: Activity = clazz.exitActivity
  def doActivity: DoActivity = clazz.doActivity

  def accept(p: Parcel): Consequence[Boolean] = clazz.accept(this, p)
  def transit(sm: StateMachine, p: Parcel): Consequence[Option[(State, Transition, Parcel)]] = clazz.transit(sm, this, p)

  def checkChageState(sm: StateMachine, p: Int): Boolean = clazz.checkChageState(sm, this, p)
}

object State {
  val finalState = State(StateClass.finalState)
}
