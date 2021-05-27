package org.goldenport.statemachine

/*
 * @since   Jan.  4, 2021
 * @version May.  3, 2021
 * @author  ASAMI, Tomoharu
 */
case class StateClass(
  name: String,
  entryActivity: Activity,
  exitActivity: Activity,
  doActivity: DoActivity,
  transitions: Vector[Transition]
) {
}
