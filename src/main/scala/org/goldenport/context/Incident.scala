package org.goldenport.context

import org.goldenport.RAISE

/*
 * See org.goldenport.incident.Incident
 * See org.goldenport.context.DetailCode.Incident
 *
 * Subtrait: Fault
 * 
 * @since   Mar.  8, 2021
 *  version Mar. 13, 2021
 * @version Apr.  3, 2022
 * @author  ASAMI, Tomoharu
 */
trait Incident {
}

case class Incidents(incidents: Vector[Incident] = Vector.empty) {
  def faults: Faults = RAISE.notImplementedYetDefect
  def effects: Effects = RAISE.notImplementedYetDefect
  def statictics: Statictics = RAISE.notImplementedYetDefect
}
object Incidents {
  val empty = Incidents()
}
