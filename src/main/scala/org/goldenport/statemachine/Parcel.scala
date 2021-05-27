package org.goldenport.statemachine

import org.goldenport.event._

/*
 * @since   May.  2, 2021
 * @version May. 25, 2021
 * @author  ASAMI, Tomoharu
 */
case class Parcel(
  context: ExecutionContext,
  event: Event
) {
  def withClass(p: StateMachineClass): Parcel = ???
}
