package org.goldenport.parser

import scala.util.Try
import scala.util.control.NonFatal
import java.net.{URL, URI}
import org.joda.time._
import org.goldenport.Strings
import org.goldenport.xsv.Lxsv
import org.goldenport.values.{Urn, NumberRange, NumberInterval, DateTimePeriod}
import org.goldenport.values.LocalDateTimeInterval
import org.goldenport.util.StringUtils
import org.goldenport.util.{DateTimeUtils, LocalDateUtils, LocalDateTimeUtils}
import org.goldenport.util.AnyUtils
import org.goldenport.util.NumberUtils
import org.goldenport.util.RegexUtils
import org.goldenport.util.{PeriodUtils, DurationUtils}
import LogicalTokens.Config
import LogicalTokens.Context

/*
 * @since   Aug. 28, 2018
 *  version Sep. 19, 2018
 *  version Oct. 26, 2018
 *  version Jan.  2, 2019
 *  version Feb. 25, 2019
 *  version Mar.  9, 2019
 *  version Jun. 30, 2019
 *  version Jul. 21, 2019
 *  version Sep. 28, 2019
 *  version Oct. 29, 2019
 *  version Nov. 28, 2019
 *  version Jan. 18, 2020
 *  version Feb. 29, 2020
 *  version Sep.  6, 2020
 *  version Oct. 12, 2020
 *  version Jan. 30, 2021
 *  version Feb. 14, 2021
 *  version Mar. 24, 2021
 *  version Apr. 21, 2021
 *  version Jun. 17, 2022
 *  version Oct. 20, 2024
 * @version Nov. 14, 2024
 * @author  ASAMI, Tomoharu
 */
sealed trait LogicalToken {
  def location: Option[ParseLocation]
  def raw: String
  def value: Any
  def string: String
  def print: String = AnyUtils.toPrint(value)
  def show: String = s"LogicalToken(${getClass.getSimpleName})#show"
  def clearLocation: LogicalToken
}
object LogicalToken {
  def buildToken(config: LogicalTokens.Config, p: Any): LogicalToken = p match {
    case m: LogicalToken => m
    case m: LogicalTokens => m.makeToken
    case "" => EmptyToken
    case m: Int => NumberToken(m)
    case m: Number => NumberToken(m)
    case m: String => StringToken(m)
    case m => StringToken(AnyUtils.toString(m))
  }

  def makeToken(config: LogicalTokens.Config, p: Any): LogicalToken =
    makeToken(LogicalTokens.Context.create(config), p)

  def makeToken(context: LogicalTokens.Context, p: Any): LogicalToken = p match {
    case m: StringToken => LogicalTokens.parse(context, m.text).makeToken
    case m: LogicalToken => m
    case m: LogicalTokens => m.makeToken
    case "" => EmptyToken
    case m: Int => NumberToken(m)
    case m: Number => NumberToken(m)
    case m: String => LogicalTokens.parse(context, m).makeToken
    case m => LogicalTokens.parse(context, AnyUtils.toString(m)).makeToken
  }
}

sealed trait LiteralToken extends LogicalToken

trait ExternalLogicalToken extends LiteralToken

case object EndToken extends LogicalToken {
  val location = None
  val raw = ""
  val value = ""
  val string = value
  override val print = ""
  def clearLocation: LogicalToken = this
}

case class EmptyToken(location: Option[ParseLocation]) extends LogicalToken {
  val raw = ""
  val value = ""
  val string = value
  override val print = ""
  def clearLocation: LogicalToken = copy(None)
}
object EmptyToken extends EmptyToken(None) {
}

case class SpaceToken(
  s: String,
  location: Option[ParseLocation]
) extends LogicalToken {
  def raw = s
  def value = s
  def string = s
  override def print = s
  def clearLocation: LogicalToken = copy(location = None)
}
object SpaceToken {
  def apply(s: String, location: ParseLocation): SpaceToken = SpaceToken(
    s,
    Some(location)
  )
}

case class DelimiterToken(
  s: String,
  location: Option[ParseLocation]
) extends LogicalToken {
  def raw = s
  def value = s
  def string = s
  override def print = s
  def clearLocation: LogicalToken = copy(location = None)
}
object DelimiterToken {
  def apply(c: Char): DelimiterToken = DelimiterToken(c.toString, None)

  def apply(c: Char, location: ParseLocation): DelimiterToken = DelimiterToken(c.toString, Some(location))

  def apply(text: String, location: ParseLocation): DelimiterToken =
    DelimiterToken(text, Some(location))
}

case class CommentToken(
  comment: String,
  location: Option[ParseLocation]
) extends LogicalToken {
  def raw = comment
  def value = comment
  def string = comment
  override def print = comment
  def clearLocation: LogicalToken = copy(location = None)
}
object CommentToken {
  def apply(text: String, location: ParseLocation): CommentToken =
    CommentToken(text, Some(location))
}

case class SingleQuoteToken(
  location: Option[ParseLocation]
) extends LogicalToken {
  val raw = "'"
  val value = "'"
  val string = value
  override val print = ""
  def clearLocation: LogicalToken = copy(location = None)
}

case class AtomToken(
  name: String,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = name
  def value = name
  def string = name
  override def print = name
  def clearLocation: LogicalToken = copy(location = None)
}
object AtomToken {
  def apply(text: String, location: ParseLocation): AtomToken =
    AtomToken(text, Some(location))
}

sealed trait StringToken extends LiteralToken {
  def prefix: Option[String]
  def value: String
  def text: String
}
object StringToken {
  def apply(p: CharSequence): StringToken = DoubleStringToken(p.toString, None)
}

case class DoubleStringToken(
  text: String,
  location: Option[ParseLocation],
  prefix: Option[String] = None
) extends StringToken {
  def raw = s""""$text""""
  def value = text
  def string = text
  override def print = text
  def clearLocation: LogicalToken = copy(location = None)
}
object DoubleStringToken {
  def apply(text: String, location: ParseLocation): DoubleStringToken =
    DoubleStringToken(text, Some(location))
}

case class SingleStringToken(
  text: String,
  location: Option[ParseLocation] = None,
  prefix: Option[String] = None
) extends StringToken {
  def raw = s"'$text'"
  def value = text
  def string = text
  override def print = text
  def clearLocation: LogicalToken = copy(location = None)
}
object SingleStringToken {
  def apply(text: String, location: ParseLocation): SingleStringToken =
    SingleStringToken(text, Some(location))
}

case class RawStringToken(
  text: String,
  location: Option[ParseLocation],
  prefix: Option[String] = None
) extends StringToken {
  def raw = "\"\"\"" + text + "\"\"\""
  def value = text
  def string = text
  override def print = text
  def clearLocation: LogicalToken = copy(location = None)
}
object RawStringToken {
  def apply(text: String, location: ParseLocation): RawStringToken =
    RawStringToken(text, Some(location))
}

case class BooleanToken(
  b: Boolean,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = b.toString // TODO
  def value = b
  def string = AnyUtils.toString(b)
  def clearLocation: LogicalToken = copy(location = None)
}

case class NumberToken(
  n: BigDecimal,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = n.toString // TODO
  def value = n
  def string = AnyUtils.toString(n)
  def clearLocation: LogicalToken = copy(location = None)
}
object NumberToken extends LogicalTokens.SimpleTokenizer {
  def apply(n: Int): NumberToken = NumberToken(BigDecimal(n), None)

  def apply(n: Number): NumberToken = NumberToken(AnyUtils.toBigDecimal(n), None)

  def apply(n: Int, location: Option[ParseLocation]): NumberToken =
    NumberToken(BigDecimal(n), location)

  def apply(n: Int, location: ParseLocation): NumberToken =
    NumberToken(BigDecimal(n), Some(location))

  override protected def accept_Token(context: Context, p: String, location: ParseLocation) =
    Try(NumberToken(BigDecimal(p), Some(location))).toOption
}

case class NumberPostfixToken(
  n: Number,
  postfix: String,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = n + postfix
  def value = raw
  def string = AnyUtils.toString(n)
  def clearLocation: LogicalToken = copy(location = None)
}
object NumberPostfixToken extends LogicalTokens.SimpleTokenizer {
  val regex = """([+-]?\d+[.]?(\d+)?([eE][+-]\d+)?)([^\d]+)""".r

  def apply(n: Int, postfix: String): NumberPostfixToken = NumberPostfixToken(n, postfix, None)

  def apply(n: Int, postfix: String, l: ParseLocation): NumberPostfixToken = NumberPostfixToken(n, postfix, Some(l))

  def apply(n: Number, postfix: String): NumberPostfixToken = NumberPostfixToken(n, postfix, None)

  override protected def accept_Token(context: Context, s: String, location: ParseLocation) =
    regex.findFirstMatchIn(s).flatMap { m =>
      val whole = m.group(0)
      if (whole == s) {
        val n = RegexUtils.parseNumber(m, 1)
        val suffix = m.group(4)
        val r = n match {
          case ParseFailure(es, ws) => ParseFailure(es, ws)
          case ParseSuccess(ast, warnings) => ParseSuccess(NumberPostfixToken(ast, suffix))
          case EmptyParseResult() => EmptyParseResult()
        }
        r.toOption
      } else {
        None
      }
    }
}

case class ComplexToken(
  n: spire.math.Complex[Double],
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = n.toString // TODO
  def value = n
  def string = AnyUtils.toString(n)
  def clearLocation: LogicalToken = copy(location = None)
}
object ComplexToken extends LogicalTokens.SimpleTokenizer {
  val regex = """([+-]?\d+[.]?(\d+)?([eE][+-]\d+)?)?([+-]\d+[.]?(\d+)?([eE][+-]\d+)?)[i]""".r

  def apply(p: spire.math.Complex[Double], l: ParseLocation): ComplexToken = ComplexToken(p, Some(l))

  def apply(real: Double, img: Double, l: ParseLocation): ComplexToken = ComplexToken(spire.math.Complex(real, img), Some(l))

  override protected def accept_Token(context: Context, s: String, location: ParseLocation) =
    regex.findFirstMatchIn(s).flatMap { m =>
      val whole = m.group(0)
      if (s == whole) {
        val real = RegexUtils.parseDouble(m, 1)
        val imaginary = RegexUtils.parseDouble(m, 4)
        val r = real match {
          case ParseFailure(es, ws) => imaginary match {
            case ParseFailure(es2, ws2) => ParseFailure(es ++ es2, ws ++ ws2)
            case _ => ParseFailure(es, ws)
          }
          case ParseSuccess(ast, warnings) => imaginary match {
            case ParseFailure(es, ws) => ParseFailure(es, warnings ++ ws)
            case ParseSuccess(ast2, ws2) => ParseSuccess(apply(ast, ast2, location))
            case EmptyParseResult() => ParseSuccess(apply(ast, 0.0, location))
          }
          case EmptyParseResult() => imaginary match {
            case ParseFailure(es, ws) => ParseFailure(es, ws)
            case ParseSuccess(ast, ws) => ParseSuccess(apply(ast, 0.0, location))
            case EmptyParseResult() => EmptyParseResult()
          }
        }
        // val real = Option(m.group(1)).map(_.toDouble).getOrElse(0.0D).toDouble
        // val imaginary = m.group(3).toDouble
        //        Some(ComplexToken(spire.math.Complex(real, imaginary), Some(location))
        r match {
          case ParseSuccess(ast, _) => Some(ast)
          case _ => None
        }
      } else {
        None
      }
    }
}
case class RationalToken(
  n: spire.math.Rational,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = n.toString // TODO
  def value = n
  def string = AnyUtils.toString(n)
  def clearLocation: LogicalToken = copy(location = None)
}
object RationalToken extends LogicalTokens.SimpleTokenizer {
  val regex = """([+-]?\d+)/([+-]?\d+)""".r

  def apply(p: spire.math.Rational, l: ParseLocation): RationalToken = RationalToken(p, Some(l))

  def apply(numerator: Long, denominator: Long, l: ParseLocation): RationalToken =
    RationalToken(spire.math.Rational(numerator, denominator), Some(l))

  override protected def accept_Token(context: Context, s: String, location: ParseLocation) =
    regex.findFirstMatchIn(s).flatMap { m =>
      val whole = m.group(0)
      if (s == whole) {
        val r = for {
          numerator <- NumberUtils.parseLong(m.group(1))
          denominator <- NumberUtils.parseLong(m.group(2))
        } yield RationalToken(spire.math.Rational(numerator, denominator), Some(location))
        r.toOption
      } else {
        None
      }
    }
}
case class RangeToken(
  range: NumberRange,
  location: Option[ParseLocation] = None,
  text: Option[String] = None
) extends LiteralToken {
  def value = range
  lazy val raw = text getOrElse range.print
  def string = AnyUtils.toString(raw)
  def clearLocation: LogicalToken = copy(location = None)
}
object RangeToken extends LogicalTokens.SimpleTokenizer {
  def apply(p: NumberRange, location: ParseLocation): RangeToken =
    RangeToken(p, Some(location))

  def apply(context: Context, s: String, location: Option[ParseLocation]): RangeToken =
    RangeToken(NumberRange.create(s), location, Some(s))

  def apply(context: Context, s: String, location: ParseLocation): RangeToken =
    RangeToken(NumberRange.create(s), Some(location), Some(s))

  def apply(start: Int, end: Int, sinc: Boolean, einc: Boolean, location: ParseLocation): RangeToken =
    RangeToken(NumberRange(start, end, sinc, einc), Some(location))

  override protected def accept_Token(context: Context, s: String, location: ParseLocation) =
    if (s.contains('~') || s.contains(','))
      NumberRange.parseOption(s).map(apply(_, location))
    else
      None
}
case class IntervalToken(
  interval: NumberInterval,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = interval.print
  def value = interval
  def string = AnyUtils.toString(value)
  def clearLocation: LogicalToken = copy(location = None)
}
object IntervalToken extends LogicalTokens.SimpleTokenizer {
  def apply(p: NumberInterval, location: ParseLocation): IntervalToken =
    IntervalToken(p, Some(location))

  def apply(context: Context, s: String, location: Option[ParseLocation]): IntervalToken =
    IntervalToken(NumberInterval.take(s), location)

  def apply(context: Context, s: String, location: ParseLocation): IntervalToken =
    IntervalToken(NumberInterval.take(s), Some(location))

  def apply(start: Int, end: Int, sinc: Boolean, einc: Boolean, location: ParseLocation): IntervalToken =
    IntervalToken(NumberInterval(start, end, sinc, einc), Some(location))

  override protected def accept_Token(context: Context, s: String, location: ParseLocation) =
    if (s.contains("~"))
      NumberInterval.parseOption(s).map(apply(_, location))
    else
      None
}

case class DateTimeToken(
  datetime: DateTime,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = AnyUtils.toPrint(datetime)
  def value = datetime
  def string = AnyUtils.toString(datetime)
  def clearLocation: LogicalToken = copy(location = None)
}
object DateTimeToken extends LogicalTokens.SimpleTokenizer {
  def apply(p: DateTime, location: ParseLocation): DateTimeToken =
    DateTimeToken(p, Some(location))

  def apply(context: Context, s: String, location: Option[ParseLocation]): DateTimeToken =
    DateTimeToken(DateTimeUtils.parseDateTimeJst(s), location) // TODO

  def apply(context: Context, s: String, location: ParseLocation): DateTimeToken =
    DateTimeToken(DateTimeUtils.parseDateTimeJst(s), Some(location)) // TODO

  def apply(y: Int, m: Int, d: Int, h: Int, mi: Int, s: Int, tz: DateTimeZone, location: ParseLocation): DateTimeToken =
    DateTimeToken(new DateTime(y, m, d, h, mi, s, tz), Some(location))

  override protected def accept_Token(context: Context, s: String, location: ParseLocation) =
    if (s.count(_ == 'T') == 1 && s.count(_ == ':') >= 2 
      && (
        s.count(_ == '-') match {
          case 2 => s.count(x => x == 'Z' || x == '+') == 1
          case 3 => !s.exists(x => x == 'Z' || x == '+')
          case _ => false
        }
      ))
      Try(apply(context, s, location)).toOption
    else
      None
}

case class LocalDateTimeToken(
  datetime: LocalDateTime,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = AnyUtils.toPrint(datetime)
  def value = datetime
  def string = AnyUtils.toString(datetime)
  def clearLocation: LogicalToken = copy(location = None)
}
object LocalDateTimeToken extends LogicalTokens.SimpleTokenizer {
  def apply(p: DateTime, location: ParseLocation): DateTimeToken =
    DateTimeToken(p, Some(location))

  def apply(context: Context, s: String, location: Option[ParseLocation]): LocalDateTimeToken =
    LocalDateTimeToken(LocalDateTimeUtils.parse(s), location)

  def apply(context: Context, s: String, location: ParseLocation): LocalDateTimeToken =
    LocalDateTimeToken(LocalDateTimeUtils.parse(s), Some(location))

  def apply(y: Int, m: Int, d: Int, h: Int, mi: Int, s: Int, location: ParseLocation): LocalDateTimeToken =
    LocalDateTimeToken(new LocalDateTime(y, m, d, h, mi, s), Some(location))

  override protected def accept_Token(context: Context, s: String, location: ParseLocation) =
    if (s.count(_ == '-') == 2 && s.count(_ == 'T') == 1 && s.contains(':') &&
      !s.exists(x => x == 'Z' || x == '+'))
      Try(apply(context, s, location)).toOption
    else
      None
}

case class LocalDateToken(
  date: LocalDate,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = AnyUtils.toPrint(date)
  def value = date
  def string = AnyUtils.toString(date)
  def clearLocation: LogicalToken = copy(location = None)
}
object LocalDateToken extends LogicalTokens.SimpleTokenizer {
  def apply(p: LocalDate, location: ParseLocation): LocalDateToken =
    LocalDateToken(p, Some(location))

  def apply(context: Context, s: String, location: Option[ParseLocation]): LocalDateToken =
    LocalDateToken(LocalDateUtils.parse(s), location)

  def apply(context: Context, s: String, location: ParseLocation): LocalDateToken =
    LocalDateToken(LocalDateUtils.parse(s), Some(location))

  def apply(y: Int, m: Int, d: Int, location: ParseLocation): LocalDateToken =
    LocalDateToken(new LocalDate(y, m, d), Some(location))

  override protected def accept_Token(context: Context, s: String, location: ParseLocation) =
    if (s.count(_ == '-') == 2 && !s.contains(':'))
      Try(apply(context, s, location)).toOption
    else
      None
}

case class LocalTimeToken(
  time: LocalTime,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = AnyUtils.toPrint(time)
  def value = time
  def string = AnyUtils.toString(time)
  def clearLocation: LogicalToken = copy(location = None)
}
object LocalTimeToken extends LogicalTokens.SimpleTokenizer {
  def apply(p: LocalTime, location: ParseLocation): LocalTimeToken =
    LocalTimeToken(p, Some(location))

  def apply(context: Context, s: String, location: Option[ParseLocation]): LocalTimeToken =
    LocalTimeToken(LocalTime.parse(s), location)

  def apply(context: Context, s: String, location: ParseLocation): LocalTimeToken =
    LocalTimeToken(LocalTime.parse(s), Some(location))

  def apply(h: Int, m: Int, s: Int, location: ParseLocation): LocalTimeToken =
    LocalTimeToken(new LocalTime(h, m, s), Some(location))

  override protected def accept_Token(context: Context, s: String, location: ParseLocation) =
    if (s.count(_ == ':') <= 2 && !s.contains('-') && !s.contains(','))
      Try(apply(context, s, location)).toOption
    else
      None
}

case class MonthDayToken(
  monthday: MonthDay,
  location: Option[ParseLocation],
  rawOption: Option[String]
) extends LiteralToken {
  def raw = rawOption getOrElse AnyUtils.toPrint(monthday)
  def value = monthday
  def string = AnyUtils.toString(monthday)
  def clearLocation: LogicalToken = copy(location = None)
}
object MonthDayToken extends LogicalTokens.SimpleTokenizer {
  def apply(p: MonthDay, location: ParseLocation): MonthDayToken =
    MonthDayToken(p, Some(location), None)

  def apply(context: Context, s: String, location: Option[ParseLocation]): MonthDayToken =
    MonthDayToken(MonthDay.parse(s), location, Some(s))

  def apply(context: Context, s: String, location: ParseLocation): MonthDayToken =
    MonthDayToken(MonthDay.parse(s), Some(location), Some(s))

  def apply(m: Int, d: Int, location: ParseLocation): MonthDayToken =
    MonthDayToken(new MonthDay(m, d), Some(location), None)

  def apply(m: Int, d: Int, location: ParseLocation, raw: String): MonthDayToken =
    MonthDayToken(new MonthDay(m, d), Some(location), None)

  val _regex = """(\d\d?)-(\d\d?)""".r

  override protected def accept_Token(context: Context, s: String, location: ParseLocation) =
    s match {
      case _regex(m, d) => Try(apply(m.toInt, d.toInt, location, s)).toOption
      case _ => None
    }
}
case class PeriodToken(
  period: Period,
  location: Option[ParseLocation],
  rawOption: Option[String]
) extends LiteralToken {
  def raw = rawOption getOrElse AnyUtils.toPrint(period)
  def value = period
  def string = raw
  def clearLocation: LogicalToken = copy(location = None)
}
object PeriodToken extends LogicalTokens.SimpleTokenizer {
  def apply(p: Period, location: ParseLocation): PeriodToken =
    PeriodToken(p, Some(location), None)

  def apply(context: Context, s: String, location: Option[ParseLocation]): PeriodToken =
    PeriodToken(Period.parse(s), location, Some(s))

  def apply(context: Context, s: String, location: ParseLocation): PeriodToken =
    PeriodToken(Period.parse(s), Some(location), Some(s))

  // private val _regex = """P((\d+)Y)?((\d+)M)?((\d+)D)?(T((\d+)H)?((\d+)M)?((\d+)S)?)?""".r

  override protected def accept_Token(context: Context, s: String, location: ParseLocation) =
    PeriodUtils.parse(s).toOption.map(PeriodToken(_, location))
  //   try {
  //   _regex.findFirstMatchIn(s).flatMap { m =>
  //     if (m.group(0) != s) {
  //       None
  //     } else {
  //       val year = _to_int(m.group(2))
  //       val month = _to_int(m.group(4))
  //       val day = _to_int(m.group(6))
  //       val hour = _to_int(m.group(9))
  //       val minute = _to_int(m.group(11))
  //       val second = _to_int(m.group(13))
  //       Some(PeriodToken(new Period(year, month, day, hour, minute, second, 0), location))
  //     }
  //   }
  // } catch {
  //   case NonFatal(e) => None
  // }

//  private def _to_int(p: String) = Option(p).map(_.toInt).getOrElse(0)

  def yearMonthDay(y: Int, m: Int, d: Int, location: ParseLocation): PeriodToken =
    PeriodToken(new Period(y, m, 0, d, 0, 0, 0, 0), location)

  def hour(h: Int, location: ParseLocation): PeriodToken =
    PeriodToken(new Period(h, 0, 0, 0), location)
}
case class DurationToken(
  duration: Duration,
  raw: String,
  location: Option[ParseLocation]
) extends LiteralToken {
  def value = duration
  def string = raw
  def clearLocation: LogicalToken = copy(location = None)
}
object DurationToken extends LogicalTokens.SimpleTokenizer {
  def apply(p: Duration, location: ParseLocation): DurationToken =
    DurationToken(p, p.toString, Some(location))

  def apply(p: Duration, s: String, location: ParseLocation): DurationToken =
    DurationToken(p, s, Some(location))

  def apply(context: Context, s: String, location: Option[ParseLocation]): DurationToken =
    DurationToken(Duration.parse(s), s, location)

  def apply(context: Context, s: String, location: ParseLocation): DurationToken =
    DurationToken(Duration.parse(s), s, Some(location))

  // private val _regex = """D((\d+)Y)?((\d+)M)?((\d+)D)?(T((\d+)H)?((\d+)M)?((\d+)S)?)?""".r

  override protected def accept_Token(context: Context, s: String, location: ParseLocation) =
    DurationUtils.parseJoda(s).toOption.map(DurationToken(_, s, location))
// try {
//     _regex.findFirstMatchIn(s).flatMap { m =>
//       if (m.group(0) != s) {
//         None
//       } else {
//         val year = _to_int(m.group(2))
//         val month = _to_int(m.group(4))
//         val day = _to_int(m.group(6))
//         val hour = _to_int(m.group(9))
//         val minute = _to_int(m.group(11))
//         val second = _to_int(m.group(13))
//         Some(DurationToken(new Duration(year, month, day, hour, minute, second, 0), location))
//       }
//     }
//   } catch {
//     case NonFatal(e) => None
//   }

  private def _to_int(p: String) = Option(p).map(_.toInt).getOrElse(0)

  def hour(h: Int, location: ParseLocation): DurationToken =
    DurationToken(Duration.standardHours(h), s"DT${h}H", location)
}
case class DateTimeIntervalToken(
  interval: DateTimePeriod,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = interval.toString // TODO
  def value = interval
  def string = AnyUtils.toString(interval)
  def clearLocation: LogicalToken = copy(location = None)
}
object DateTimeIntervalToken extends LogicalTokens.SimpleTokenizer {
  def apply(p: DateTimePeriod, location: ParseLocation): DateTimeIntervalToken =
    DateTimeIntervalToken(p, Some(location))

  def apply(context: Context, s: String, location: Option[ParseLocation]): DateTimeIntervalToken =
    DateTimeIntervalToken(DateTimePeriod.parse(s), location)

  def apply(context: Context, s: String, location: ParseLocation): DateTimeIntervalToken =
    DateTimeIntervalToken(DateTimePeriod.parse(s), Some(location))

  def atOrAbove(y: Int, m: Int, d: Int, h: Int, mi: Int, s: Int, tz: DateTimeZone, location: ParseLocation): DateTimeIntervalToken =
    DateTimeIntervalToken(DateTimePeriod.atOrAbove(y, m, d, h, mi, s, tz), Some(location))

  override protected def accept_Token(context: Context, s: String, location: ParseLocation) =
    if (_is_datetime_interval(s))
      DateTimePeriod.parseOption(s).map(apply(_, location)) // TODO timezone, datetime)
    else
      None

  private def _is_datetime_interval(p: String) = {
    Strings.totokens(p, "~") match {
      case Nil => false
      case x :: Nil => _is_datetime(x)
      case x :: y :: Nil => _is_datetime(x) && _is_datetime(y)
      case _ => false
    }
  }

  private val _regex = """(\d\d)-(\d\d)-(\d\d)T(\d\d):(\d\d):(\d\d)([Z+-])(.*)""".r

  private def _is_datetime(p: String) = _regex.findFirstMatchIn(p).isDefined
}

case class LocalDateTimeIntervalToken(
  interval: LocalDateTimeInterval,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = interval.toString // TODO
  def value = interval
  def string = AnyUtils.toString(interval)
  def clearLocation: LogicalToken = copy(location = None)
}
object LocalDateTimeIntervalToken extends LogicalTokens.SimpleTokenizer {
  def apply(p: LocalDateTimeInterval, location: ParseLocation): LocalDateTimeIntervalToken =
    LocalDateTimeIntervalToken(p, Some(location))

  def apply(context: Context, s: String, location: Option[ParseLocation]): LocalDateTimeIntervalToken =
    LocalDateTimeIntervalToken(LocalDateTimeInterval.create(s), location)

  def apply(context: Context, s: String, location: ParseLocation): LocalDateTimeIntervalToken =
    LocalDateTimeIntervalToken(LocalDateTimeInterval.create(s), Some(location))

  def atOrAbove(y: Int, m: Int, d: Int, h: Int, mi: Int, s: Int, location: ParseLocation): LocalDateTimeIntervalToken =
    LocalDateTimeIntervalToken(LocalDateTimeInterval.atOrAbove(y, m, d, h, mi, s), Some(location))

  override protected def accept_Token(context: Context, s: String, location: ParseLocation) =
    if (s.contains("~"))
      LocalDateTimeInterval.parseOption(context.dateTimeContext, s).map(apply(_, location)) // TODO timezone, datetime
    else
      None
}

case class UrlToken(
  url: URL,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = url.toString // TODO
  def value = url
  def string = AnyUtils.toString(url)
  def clearLocation: LogicalToken = copy(location = None)
}
object UrlToken extends LogicalTokens.SimpleTokenizer {
  def apply(s: String, location: Option[ParseLocation]): UrlToken =
    UrlToken(new URL(s), location)

  def apply(s: String, location: ParseLocation): UrlToken =
    UrlToken(new URL(s), Some(location))

  override protected def accept_Token(context: Context, p: String, location: ParseLocation) =
    Try(apply(p, location)).toOption

  private val _url_schemes = Vector("http", "https", "file")

  def isUrl(p: String): Boolean = Strings.totokens(p, ":") match {
    case Nil => false
    case x :: Nil => false
    case x :: _ => _url_schemes.contains(x)
  }
}

case class UrnToken(
  urn: Urn,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = urn.toString // TODO
  def value = urn
  def string = AnyUtils.toString(urn)
  def clearLocation: LogicalToken = copy(location = None)
}
object UrnToken extends LogicalTokens.SimpleTokenizer {
  def apply(s: String, location: Option[ParseLocation]): UrnToken =
    UrnToken(Urn(s), location)

  def apply(s: String, location: ParseLocation): UrnToken =
    UrnToken(Urn(s), Some(location))

  override protected def accept_Token(context: Context, p: String, location: ParseLocation) =
    if (isUrn(p))
      Try(apply(p, location)).toOption
    else
      None

  def isUrn(p: String) = UriToken.isUri(p) && (Strings.totokens(p, ":") match {
    case Nil => false
    case s :: _ => s.equalsIgnoreCase("urn")
    case _ => false
  })
}

case class UriToken(
  uri: URI,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = uri.toString // TODO
  def value = uri
  def string = AnyUtils.toString(uri)
  def clearLocation: LogicalToken = copy(location = None)
}
object UriToken extends LogicalTokens.SimpleTokenizer {
  def apply(s: String, location: Option[ParseLocation]): UriToken =
    UriToken(new URI(s), location)

  def apply(s: String, location: ParseLocation): UriToken =
    UriToken(new URI(s), Some(location))

  override protected def accept_Token(context: Context, p: String, location: ParseLocation) =
    if (isUri(p))
      Try(apply(p, location)).toOption
    else
      None

  def isUri(p: String) = Strings.totokens(p, ":") match {
    case Nil => false
    case x :: Nil => false
    case x :: y :: _ => !x.forall(_.isDigit) && !y.startsWith("//") // datetime, url
  }
}

case class PathToken(
  path: String,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = path
  def value = path
  def string = path
  def clearLocation: LogicalToken = copy(location = None)
}
object PathToken extends LogicalTokens.SimpleTokenizer {
  def apply(s: String, location: ParseLocation): PathToken =
    PathToken(s, Some(location))

  override protected def accept_Token(context: Context, p: String, location: ParseLocation) =
    if (_is_path(p))
      Try(apply(p, location)).toOption
    else
      None

  private def _is_path(p: String) = p.length > 1 && (p match {
    case _ => (p.startsWith("@") || p.contains('/'))
  })
}

case class ExpressionToken(
  text: String,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = text
  def value = text
  def string = text
  override def print = text
  def clearLocation: LogicalToken = copy(location = None)
}
object ExpressionToken extends LogicalTokens.SimpleTokenizer {
  def apply(text: String, location: ParseLocation): ExpressionToken =
    ExpressionToken(text, Some(location))

  override protected def accept_Token(context: Context, p: String, location: ParseLocation) =
    if (_is_expression(p))
      Try(apply(p, location)).toOption
    else
      None

  // TODO function name with hypen (e.g. my-func) is allowed.
  private def _is_expression(p: String) =
    _is_not_atom(p) && p.headOption.
      map(x =>
        p.length match {
          case 0 => false
          case 1 => _is_accept_first_only(x)
          case _ => _is_accept_first(x) && p.tail.forall(_is_accept)
        }
      ).getOrElse(false)

  @inline
  private def _is_atom(p: String) = (
    StringUtils.isLispIdentifierI18N(p) || StringUtils.isNumericalSymbol(p)
      || p == "." || p == '%'
  )

  @inline
  private def _is_not_atom(p: String) = !_is_atom(p)

  // # : history
  // ? : stack
  // ! : command
  // . : stack head
  @inline
  private def _is_accept_first(c: Char) =
    StringUtils.isLispIdentifierI18NChar(c) || c == '#' || c == '?' || c == '!' || c == '.'

  @inline
  private def _is_accept_first_only(c: Char) = StringUtils.isLispIdentifierI18NChar(c)

  // '(', '{', '[' and '@' : in first character, used by another token type.
  // '/' : used by PathToken.
  @inline
  private def _is_accept(c: Char) = {
    val r = StringUtils.isLispIdentifierI18NChar(c) || StringUtils.isScriptSymbolChar(c) || c == '%'
//    println(s"ExpressionToken#_is_accept $c => $r")
    r
  }
}

case class XsvToken(
  text: String,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = text
  def value = text // TODO
  def string = text
  override def print = text
  def clearLocation: LogicalToken = copy(location = None)
}
object XsvToken extends LogicalTokens.SimpleTokenizer {
  def apply(s: String, location: ParseLocation): XsvToken =
    XsvToken(s, Some(location))

  override protected def accept_Token(context: Context, p: String, location: ParseLocation) =
    if (_is_xsv(p))
      Try(apply(p, location)).toOption
    else
      None

  private def _is_xsv(p: String) = false // TODO
}

case class LxsvToken(
  text: String,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = text
  def value = text
  def string = text
  override def print = text
  def clearLocation: LogicalToken = copy(location = None)
  def lxsv: Lxsv = Lxsv.create(text)
}
object LxsvToken extends LogicalTokens.SimpleTokenizer {
  def apply(s: String, location: ParseLocation): LxsvToken =
    LxsvToken(s, Some(location))

  override protected def accept_Token(context: Context, p: String, location: ParseLocation) =
    if (_is_lxsv(p))
      Try(apply(p, location)).toOption
    else
      None

  private def _is_lxsv(p: String): Boolean = Lxsv.isExplicitLxsvToken(p)
}

case class BracketToken(
  text: String,
  location: Option[ParseLocation],
  prefix: Option[String] = None
) extends LiteralToken {
  def raw = s"[$text]" // TODO
  def value = text
  def string = text
  override def print = text
  def clearLocation: LogicalToken = copy(location = None)
}
object BracketToken {
  def apply(text: String, location: ParseLocation): BracketToken =
    BracketToken(text, Some(location))
}

case class DoubleBracketToken(
  text: String,
  location: Option[ParseLocation],
  prefix: Option[String] = None
) extends LiteralToken {
  def raw = s"[[$text]]" // TODO
  def value = text
  def string = text
  override def print = text
  def clearLocation: LogicalToken = copy(location = None)
}
object DoubleBracketToken {
  def apply(text: String, location: ParseLocation): DoubleBracketToken =
    DoubleBracketToken(text, Some(location))
}

case class RawBracketToken(
  text: String,
  location: Option[ParseLocation],
  prefix: Option[String] = None
) extends LiteralToken {
  def raw = s"[[[$text]]]" // TODO
  def value = text
  def string = text
  override def print = text
  def clearLocation: LogicalToken = copy(location = None)
}
object RawBracketToken {
  def apply(text: String, location: ParseLocation): RawBracketToken =
    RawBracketToken(text, Some(location))
}

/*
 * prefix$[properties]{text}%{postfix}
 */
case class ScriptToken(
  text: String,
  location: Option[ParseLocation],
  prefix: Option[String] = None,
  properties: Option[String] = None,
  postfix: Option[String] = None
) extends LiteralToken {
  def raw = "${" + text + "}" // TODO
  def value = text
  def string = text
  override def print = text
  def clearLocation: LogicalToken = copy(location = None)
}
object ScriptToken {
  def apply(text: String, location: ParseLocation): ScriptToken =
    ScriptToken(text, Some(location))

  def apply(text: String): ScriptToken =
    ScriptToken(text, None)

  def apply(text: String, properties: Option[String], postfix: Option[String]): ScriptToken =
    ScriptToken(text, None, None, properties, postfix)
}

case class ExplicitLiteralToken(
  text: String,
  location: Option[ParseLocation],
  prefix: Option[String] = None
) extends LiteralToken {
  def raw = text
  def value = text
  def string = text
  override def print = text
  def clearLocation: LogicalToken = copy(location = None)
}
object ExplicitLiteralToken {
  def apply(text: String, location: ParseLocation): LiteralToken =
    ExplicitLiteralToken(text, Some(location))
}
