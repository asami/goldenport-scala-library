package org.goldenport.event

import org.goldenport.collection.NonEmptyVector

/*
 * @since   Jan.  4, 2021
 *  version May. 29, 2021
 *  version Jun. 14, 2021
 * @version Aug. 22, 2022
 * @author  ASAMI, Tomoharu
 */
trait Event {
  def clazz: EventClazz
  def name: String = clazz.name
  def content: Event.Content

  def isResourceId(id: String): Boolean = content.isResourceId(id)
}

object Event {
  case class Content(
    resourceIds: Option[NonEmptyVector[String]] = None
  ) {
    def isResourceId(id: String): Boolean =
      resourceIds.map(_.contains(id)).getOrElse(false)
  }
  object Content {
    val empty = Content()
  }
}

case class CallEvent(
  clazz: EventClazz,
  call: Call,
  content: Event.Content = Event.Content.empty
) extends Event {
  def to: ObjectId = call.to
}

case class ChangeEvent(
  clazz: EventClazz,
  change: Change,
  content: Event.Content = Event.Content.empty
) extends Event {
}

case class SignalEvent(
  clazz: EventClazz,
  signal: Signal,
  content: Event.Content = Event.Content.empty
) extends Event {
}
object SignalEvent {
  def create(p: Signal): SignalEvent = {
    val c = EventClazz(p.name)
    SignalEvent(c, p)
  }
}

case class TimeEvent(
  clazz: EventClazz,
  time: Instant,
  content: Event.Content = Event.Content.empty
) extends Event {
}

trait ControlEvent extends Event {
  def content: Event.Content = Event.Content.empty
}

case object InitEvent extends ControlEvent {
  val clazz = EventClazz("system.control.init")
}

case object GoAheadEvent extends ControlEvent {
  val clazz = EventClazz("system.control.go-ahead")
}
