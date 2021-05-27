package org.goldenport.statemachine

/*
 * @since   Jan.  4, 2021
 * @version May. 24, 2021
 * @author  ASAMI, Tomoharu
 */
case class State(
  clazz: StateClass
  // major: MajorState,
  // minor: MinorState
) {
  def entryActivity: Activity = clazz.entryActivity
  def exitActivity: Activity = clazz.exitActivity
  def doActivity: DoActivity = clazz.doActivity
}
