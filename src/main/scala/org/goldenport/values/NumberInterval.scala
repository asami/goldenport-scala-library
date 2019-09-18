package org.goldenport.values

import scala.util.Try
import spire.math.Interval
import spire.math.interval._
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.extension.Showable
import org.goldenport.util.AnyRefUtils

/*
 * @since   Sep. 12, 2019
 * @version Sep. 14, 2019
 * @author  ASAMI, Tomoharu
 */
case class NumberInterval(interval: Interval[spire.math.Number]) extends Showable {
  def print = {
    val start = _to_bound(interval.lowerBound)
    val end = _to_bound(interval.upperBound)
    s"$start~$end"
  }

  private def _to_bound(p: Bound[spire.math.Number]) = p match {
    case Open(a) => AnyRefUtils.toString(a) + "!"
    case Closed(a) => AnyRefUtils.toString(a)
    case Unbound() => ""
    case EmptyBound() => ""
  }

  def display = print
  def show = print

  def toRange: NumberRange = {
    val (start, si) = _to_number(interval.lowerBound)
    val (end, ei) = _to_number(interval.upperBound)
    (start, end) match {
      case (Some(s), Some(e)) => RepeatRange(s, e, si, ei)
      case (Some(s), None) => if (si) ValueRange(s) else NoneRange
      case (None, Some(e)) => if (ei) ValueRange(e) else NoneRange
      case (None, None) => NoneRange
    }
  }

  private def _to_number(p: Bound[spire.math.Number]) = p match {
    case Open(a) => (Some(a), false)
    case Closed(a) => (Some(a), true)
    case Unbound() => (None, false)
    case EmptyBound() => (None, false)
  }
}

object NumberInterval {
  private val _regex = """([^~^!]+)?(!)?~([^+^-^!]+)?(!)?""".r

  def parseOption(p: String): Option[NumberInterval] = Try(parse(p)).toOption

  def parse(p: String): NumberInterval = {
    val _regex(start, sinc, end, einc) = p
    val r = _parse(
      Option(start).map(x => AnyRefUtils.toSpireNumber(x)),
      Option(sinc).isEmpty,
      Option(end).map(x => AnyRefUtils.toSpireNumber(x)),
      Option(einc).isEmpty
    )
    NumberInterval(r)
  }

  import spire.math._
  import spire.math.interval._

  private def _parse(start: Option[Number], sinc: Boolean, end: Option[Number], einc: Boolean) = {
    val low: Bound[Number] = start.map(_to_b(_, sinc)).getOrElse(Unbound())
    val high: Bound[Number] = end.map(_to_b(_, einc)).getOrElse(Unbound())
    Interval.fromBounds(low, high)
  }

  private def _to_b(p: Number, inclusive: Boolean): Bound[Number] =
    if (inclusive)
      Closed(p)
    else
      Open(p)
}
