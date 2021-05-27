package org.goldenport.statemachine

/*
 * @since   Jan.  4, 2021
 * @version May. 25, 2021
 * @author  ASAMI, Tomoharu
 */
case class Transition(
  guard: SmGuard,
  activity: Activity,
  to: TransitionTo
) {
}
