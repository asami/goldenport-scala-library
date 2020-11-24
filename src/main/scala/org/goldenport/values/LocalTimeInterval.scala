package org.goldenport.values

import org.joda.time.LocalTime
import spire.math.Interval
import spire.math.interval._
import org.goldenport.parser.ParseResult

/*
 * @since   Sep.  3, 2020
 * @version Sep.  6, 2020
 * @author  ASAMI, Tomoharu
 */
case class LocalTimeInterval(interval: Interval[LocalTime]) extends IntervalBase[LocalTime] {
  import LocalTimeInterval._

  def toIntervalLocalTime: Interval[LocalTime] = interval
}

object LocalTimeInterval extends IntervalFactory[LocalTimeInterval, LocalTime] {
  implicit val _order: spire.algebra.Order[LocalTime] = new spire.algebra.Order[LocalTime] {
    def compare(lhs: LocalTime, rhs: LocalTime) = lhs.compareTo(rhs)
  }

  protected def to_Value(p: String): ParseResult[LocalTime] =
    ParseResult(LocalTime.parse(p))

  protected def to_Interval(p: Interval[LocalTime]): ParseResult[LocalTimeInterval] =
    ParseResult(LocalTimeInterval(p))
}
