package org.goldenport.values

import scala.util.Try
import spire.math.Interval
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.util.AnyRefUtils

/*
 * @since   Sep. 12, 2019
 * @version Sep. 12, 2019
 * @author  ASAMI, Tomoharu
 */
case class NumberInterval(interval: Interval[spire.math.Number]) {
}

object NumberInterval {
  private val _regex = """([^~^!]+)?(!)?~([^+^-^!]+)?(!)?""".r

  def parseOption(p: String): Option[NumberInterval] = Try(parse(p)).toOption

  def parse(p: String): NumberInterval = {
    val _regex(start, sinc, end, einc) = p
    val r = _parse(
      Option(start).map(x => AnyRefUtils.toSpireNumber(x)),
      Option(sinc).isDefined,
      Option(end).map(x => AnyRefUtils.toSpireNumber(x)),
      Option(einc).isDefined
    )
    NumberInterval(r)
  }

  import spire.math._
  import spire.math.interval._

  private def _parse(start: Option[Number], sinc: Boolean, end: Option[Number], einc: Boolean) = {
    val low: Bound[Number] = start.map(_to_b(_, sinc)).getOrElse(Unbound())
    val high: Bound[Number] = start.map(_to_b(_, einc)).getOrElse(Unbound())
    Interval.fromBounds(low, high)
  }

  private def _to_b(p: Number, inclusive: Boolean): Bound[Number] =
    if (inclusive)
      Closed(p)
    else
      Open(p)
}
