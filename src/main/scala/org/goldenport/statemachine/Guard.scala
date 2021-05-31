package org.goldenport.statemachine

/*
 * @since   May.  3, 2021
 * @version May. 29, 2021
 * @author  ASAMI, Tomoharu
 */
trait SmGuard {
  def isGoAhead: Boolean = false
  def accept(p: Parcel): Boolean
}

object Guard {
  // case class All() extends Guard {
  // }
}

case object AllGuard extends SmGuard {
  override def isGoAhead = true
  def accept(p: Parcel): Boolean = true
}

case class EventNameGuard(name: String) extends SmGuard {
  def accept(p: Parcel): Boolean = p.event.name == name
}
