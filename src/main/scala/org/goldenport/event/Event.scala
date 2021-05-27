package org.goldenport.event

/*
 * @since   Jan.  4, 2021
 * @version May. 24, 2021
 * @author  ASAMI, Tomoharu
 */
trait Event {
  def clazz: EventClazz
  def name: String = clazz.name
}

object Event {
}

case class CallEvent(
  clazz: EventClazz,
  call: Call
) extends Event {
}

case class ChangeEvent(
  clazz: EventClazz,
  change: Change
) extends Event {
}

case class SignalEvent(
  clazz: EventClazz,
  signal: Signal
) extends Event {
}

case class TimeEvent(
  clazz: EventClazz,
  time: Instant
) extends Event {
}

trait ControlEvent extends Event {
}

case object GoAheadEvent extends ControlEvent {
  val clazz = EventClazz("system.control.go-ahead")
}
