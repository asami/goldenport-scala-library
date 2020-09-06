package org.goldenport.values

import scala.util.Try
import spire.math.Interval
import spire.math.interval._
import org.goldenport.RAISE
import org.goldenport.parser.ParseResult
import org.goldenport.util.AnyRefUtils
// import org.goldenport.Strings
// import org.goldenport.extension.Showable
// import org.goldenport.util.AnyRefUtils

/*
 * @since   Sep. 12, 2019
 *  version Oct. 16, 2019
 * @version Sep.  6, 2020
 * @author  ASAMI, Tomoharu
 */
case class NumberInterval(interval: Interval[spire.math.Number]) extends IntervalBase[spire.math.Number] {
  // private def _to_bound(p: Bound[spire.math.Number]) = p match {
  //   case Open(a) => AnyRefUtils.toString(a) + "!"
  //   case Closed(a) => AnyRefUtils.toString(a)
  //   case Unbound() => ""
  //   case EmptyBound() => ""
  // }

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

  def toIntervalBigDecimal: Interval[BigDecimal] = {
    import spire.implicits._
    interval.mapBounds(_.toBigDecimal)
  }
}

object NumberInterval extends IntervalFactory[NumberInterval, spire.math.Number] {
  implicit val _order: spire.algebra.Order[spire.math.Number] = new spire.algebra.Order[spire.math.Number] {
    def compare(lhs: spire.math.Number, rhs: spire.math.Number) = lhs.compare(rhs)
  }

  protected def to_Value(p: String): ParseResult[spire.math.Number] =
    ParseResult(AnyRefUtils.toSpireNumber(p))

  protected def to_Interval(p: Interval[spire.math.Number]): ParseResult[NumberInterval] =
    ParseResult(NumberInterval(p))
}

// object NumberInterval0 {
//   val MARK_OPEN = "!"
//   val MARK_CLOSE = "#"

//   private val _regex = """([^~^!^#]+)?([!#])?~([^+^-^!^#]+)?([!#])?""".r

//   def parseOption(p: String): Option[NumberInterval] = Try(parse(p)).toOption

//   def parse(p: String): NumberInterval = {
//     val _regex(start, sinc, end, einc) = p
//     val r = _parse(
//       Option(start).map(x => AnyRefUtils.toSpireNumber(x)),
//       _include_close(sinc),
//       Option(end).map(x => AnyRefUtils.toSpireNumber(x)),
//       _include_close(einc)
//     )
//     NumberInterval(r)
//   }

//   private def _include_open(p: String) = Option(p).map(_is_close) getOrElse false
//   private def _include_close(p: String) = Option(p).map(_is_close) getOrElse true

//   private def _is_close(p: String) = p match {
//     case MARK_CLOSE => true
//     case MARK_OPEN => false
//     case m => RAISE.syntaxErrorFault(s"Invalid open/close: $p")
//   }

//   import spire.math._
//   import spire.math.interval._

//   private def _parse(start: Option[Number], sinc: Boolean, end: Option[Number], einc: Boolean) = {
//     val low: Bound[Number] = start.map(_to_b(_, sinc)).getOrElse(Unbound())
//     val high: Bound[Number] = end.map(_to_b(_, einc)).getOrElse(Unbound())
//     Interval.fromBounds(low, high)
//   }

//   private def _to_b(p: Number, inclusive: Boolean): Bound[Number] =
//     if (inclusive)
//       Closed(p)
//     else
//       Open(p)

//   def parseCloseOpen(p: String): NumberInterval = {
//     val _regex(start, sinc, end, einc) = p
//     val r = _parse(
//       Option(start).map(x => AnyRefUtils.toSpireNumber(x)),
//       _include_close(sinc),
//       Option(end).map(x => AnyRefUtils.toSpireNumber(x)),
//       _include_open(sinc)
//     )
//     NumberInterval(r)
//   }
// }
