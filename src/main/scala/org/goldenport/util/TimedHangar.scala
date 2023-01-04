package org.goldenport.util

import scala.concurrent.duration._
import java.sql.Timestamp
import org.joda.time.DateTime
import org.goldenport.context.{DateTimeContext => CDateTimeContext}

/*
 * @since   Nov. 13, 2022
 *  version Nov. 13, 2022
 * @version Jan.  4, 2023
 * @author  ASAMI, Tomoharu
 */
class TimedHangar[T](
  val context: CDateTimeContext,
  val defaultDuration: Duration,
  val obsolatedDuration: Duration = Duration.Inf
) {
  import TimedHangar._

  private var _slot: Slot[T] = Slot.Empty()

  def get: Option[T] = {
    val (s, v) = _slot.get(context.current)
    _slot = s
    v
  }

  def getDeprecated: Option[T] = {
    val (s, v) = _slot.getDeprecated(context.current)
    _slot = s
    v
  }

  def getObsolated: Option[T] = {
    val (s, v) = _slot.getObsolated(context.current)
    _slot = s
    v
  }

  def set(p: T): Unit = {
    _slot = Slot.Active(context.timestamp, p, defaultDuration, obsolatedDuration)
  }
  def set(p: T, duration: Duration): Unit = {
    _slot = Slot.Active(context.timestamp, p, duration, obsolatedDuration)
  }
  def setSeconds(p: T, duration: Int): Unit = set(p, Duration(duration, SECONDS))
}

object TimedHangar {
  def create[T](): TimedHangar[T] = new TimedHangar(CDateTimeContext.now(), Duration.Inf)
  def create[T](duration: Duration): TimedHangar[T] = new TimedHangar(CDateTimeContext.now(), duration)
  def createDays[T](p: Int): TimedHangar[T] = create(Duration(p, DAYS))
  def createHours[T](p: Int): TimedHangar[T] = create(Duration(p, HOURS))
  def createMinutes[T](p: Int): TimedHangar[T] = create(Duration(p, MINUTES))
  def createSeconds[T](p: Int): TimedHangar[T] = create(Duration(p, SECONDS))
  def createMilliSeconds[T](p: Int): TimedHangar[T] = create(Duration(p, MILLISECONDS))

  sealed trait Slot[T] {
    def get(current: DateTime): (Slot[T], Option[T])
    def getDeprecated(current: DateTime): (Slot[T], Option[T])
    def getObsolated(current: DateTime): (Slot[T], Option[T])

    protected final def is_available(
      current: DateTime,
      ts: Timestamp,
      duration: Duration
    ): Boolean =
      if (duration == Duration.Inf)
        true
      else
        current.getMillis <= ts.getTime + duration.toMillis
  }
  object Slot {
    case class Empty[T]() extends Slot[T] {
      def get(current: DateTime): (Slot[T], Option[T]) = (this, None)
      def getDeprecated(current: DateTime): (Slot[T], Option[T]) = (this, None)
      def getObsolated(current: DateTime): (Slot[T], Option[T]) = (this, None)
    }

    case class Active[T](
      timestamp: Timestamp,
      value: T,
      duration: Duration,
      obsolatedDuration: Duration
    ) extends Slot[T] {
      def get(current: DateTime): (Slot[T], Option[T]) =
        if (is_available(current, timestamp, duration))
          (this, Some(value))
        else if (is_available(current, timestamp, obsolatedDuration))
          (Deprecated(timestamp, value, obsolatedDuration), None)
        else
          (Obsolated(timestamp, value), None)

      def getDeprecated(current: DateTime): (Slot[T], Option[T]) =
        if (is_available(current, timestamp, duration))
          (this, Some(value))
        else if (is_available(current, timestamp, obsolatedDuration))
          (Deprecated(timestamp, value, obsolatedDuration), Some(value))
        else
          (Obsolated(timestamp, value), None)

      def getObsolated(current: DateTime): (Slot[T], Option[T]) =
        if (is_available(current, timestamp, duration))
          (this, Some(value))
        else if (is_available(current, timestamp, obsolatedDuration))
          (Deprecated(timestamp, value, obsolatedDuration), Some(value))
        else
          (Obsolated(timestamp, value), Some(value))
    }

    case class Deprecated[T](
      timestamp: Timestamp,
      value: T,
      obsolatedDuration: Duration
    ) extends Slot[T] {
      def get(current: DateTime): (Slot[T], Option[T]) = 
        if (is_available(current, timestamp, obsolatedDuration))
          (this, None)
        else
          (Obsolated(timestamp, value), None)

      def getDeprecated(current: DateTime): (Slot[T], Option[T]) =
        if (is_available(current, timestamp, obsolatedDuration))
          (this, Some(value))
        else
          (Obsolated(timestamp, value), None)

      def getObsolated(current: DateTime): (Slot[T], Option[T]) =
        if (is_available(current, timestamp, obsolatedDuration))
          (this, Some(value))
        else
          (Obsolated(timestamp, value), Some(value))
    }

    case class Obsolated[T](
      timestamp: Timestamp,
      value: T
    ) extends Slot[T] {
      def get(current: DateTime): (Slot[T], Option[T]) = (this, None)
      def getDeprecated(current: DateTime): (Slot[T], Option[T]) = (this, None)
      def getObsolated(current: DateTime): (Slot[T], Option[T]) = (this, Some(value))
    }
  }
}
