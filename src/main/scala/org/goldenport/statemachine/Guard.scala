package org.goldenport.statemachine

/*
 * @since   May.  3, 2021
 * @version May. 24, 2021
 * @author  ASAMI, Tomoharu
 */
trait SmGuard {
  def isGoAhead: Boolean = false
}

object Guard {
  // case class All() extends Guard {
  // }
}

case object AllGuard extends SmGuard {
  override def isGoAhead = true
}

case class EventNameGuard(name: String) extends SmGuard {
}
