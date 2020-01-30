package org.goldenport.parser

import scala.util.Try
import java.net.{URL, URI}
import org.joda.time._
import org.goldenport.Strings
import org.goldenport.xsv.Lxsv
import org.goldenport.values.{Urn, NumberRange, NumberInterval, DateTimePeriod}
import org.goldenport.util.StringUtils
import org.goldenport.util.{DateTimeUtils, LocalDateUtils, LocalDateTimeUtils}
import org.goldenport.util.AnyUtils
import LogicalTokens.Config

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
 * @version Jan. 18, 2020
 * @author  ASAMI, Tomoharu
 */
sealed trait LogicalToken {
  def location: Option[ParseLocation]
  def raw: String
  def value: Any
  def print: String = AnyUtils.toString(value)
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

  def makeToken(config: LogicalTokens.Config, p: Any): LogicalToken = p match {
    case m: StringToken => LogicalTokens.parse(config, m.text).makeToken
    case m: LogicalToken => m
    case m: LogicalTokens => m.makeToken
    case "" => EmptyToken
    case m: Int => NumberToken(m)
    case m: Number => NumberToken(m)
    case m: String => LogicalTokens.parse(config, m).makeToken
    case m => LogicalTokens.parse(config, AnyUtils.toString(m)).makeToken
  }
}

sealed trait LiteralToken extends LogicalToken

trait ExternalLogicalToken extends LiteralToken

case object EndToken extends LogicalToken {
  val location = None
  val raw = ""
  val value = ""
  override val print = ""
  def clearLocation: LogicalToken = this
}

case class EmptyToken(location: Option[ParseLocation]) extends LogicalToken {
  val raw = ""
  val value = ""
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
  override def print = s
  def clearLocation: LogicalToken = copy(location = None)
}
object DelimiterToken {
  def apply(c: Char): DelimiterToken = DelimiterToken(c.toString, None)

  def apply(c: Char, location: ParseLocation): DelimiterToken = DelimiterToken(c.toString, Some(location))

  def apply(text: String, location: ParseLocation): DelimiterToken =
    DelimiterToken(text, Some(location))
}

case class SingleQuoteToken(
  location: Option[ParseLocation]
) extends LogicalToken {
  val raw = "'"
  val value = "'"
  override val print = ""
  def clearLocation: LogicalToken = copy(location = None)
}

case class AtomToken(
  name: String,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = name
  def value = name
  override def print = name
  def clearLocation: LogicalToken = copy(location = None)
}
object AtomToken {
  def apply(text: String, location: ParseLocation): AtomToken =
    AtomToken(text, Some(location))
}

sealed trait StringToken extends LiteralToken {
  def prefix: Option[String]
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
  def clearLocation: LogicalToken = copy(location = None)
}

case class NumberToken(
  n: BigDecimal,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = n.toString // TODO
  def value = n
  def clearLocation: LogicalToken = copy(location = None)
}
object NumberToken extends LogicalTokens.SimpleTokenizer {
  def apply(n: Int): NumberToken = NumberToken(BigDecimal(n), None)

  def apply(n: Number): NumberToken = NumberToken(AnyUtils.toBigDecimal(n), None)

  def apply(n: Int, location: Option[ParseLocation]): NumberToken =
    NumberToken(BigDecimal(n), location)

  def apply(n: Int, location: ParseLocation): NumberToken =
    NumberToken(BigDecimal(n), Some(location))

  override protected def accept_Token(config: Config, p: String, location: ParseLocation) =
    Try(NumberToken(BigDecimal(p), Some(location))).toOption
}

case class ComplexToken(
  n: spire.math.Complex[Double],
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = n.toString // TODO
  def value = n
  def clearLocation: LogicalToken = copy(location = None)
}
object ComlexToken extends LogicalTokens.SimpleTokenizer {
  val regex = """([+-]?\d+[.]?\d+([eE][+-]\d+)?)?([+-]\d+[.]?\d+([eE][+-]\d+)?)[i]""".r

  def apply(p: spire.math.Complex[Double], l: ParseLocation): ComplexToken = ComplexToken(p, Some(l))

  override protected def accept_Token(config: Config, s: String, location: ParseLocation) =
    regex.findFirstMatchIn(s).flatMap { m =>
      val whole = m.group(0)
      if (s == whole) {
        val real = Option(m.group(1)).map(_.toDouble).getOrElse(0.0D).toDouble
        val imaginary = m.group(3).toDouble
        Some(ComplexToken(spire.math.Complex(real, imaginary), Some(location)))
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
  def clearLocation: LogicalToken = copy(location = None)
}
case class RangeToken(
  range: NumberRange,
  location: Option[ParseLocation],
  raw: String
) extends LiteralToken {
  def value = range
  def clearLocation: LogicalToken = copy(location = None)
}
object RangeToken extends LogicalTokens.SimpleTokenizer {
  def apply(p: NumberRange, location: ParseLocation): RangeToken =
    RangeToken(p, Some(location), p.print)

  def apply(config: Config, s: String, location: Option[ParseLocation]): RangeToken =
    RangeToken(NumberRange.parse(s), location, s)

  def apply(config: Config, s: String, location: ParseLocation): RangeToken =
    RangeToken(NumberRange.parse(s), Some(location), s)

  override protected def accept_Token(config: Config, s: String, location: ParseLocation) =
    if (s.contains('~') || s.contains(','))
      NumberRange.parseOption(s).map(apply(_, location))
    else
      None
}
case class IntervalToken(
  interval: NumberInterval,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = interval.toString // TODO
  def value = interval
  def clearLocation: LogicalToken = copy(location = None)
}
object IntervalToken extends LogicalTokens.SimpleTokenizer {
  def apply(p: NumberInterval, location: ParseLocation): IntervalToken =
    IntervalToken(p, Some(location))

  def apply(config: Config, s: String, location: Option[ParseLocation]): IntervalToken =
    IntervalToken(NumberInterval.parse(s), location)

  def apply(config: Config, s: String, location: ParseLocation): IntervalToken =
    IntervalToken(NumberInterval.parse(s), Some(location))

  override protected def accept_Token(config: Config, s: String, location: ParseLocation) =
    if (s.contains("~"))
      NumberInterval.parseOption(s).map(apply(_, location))
    else
      None
}

case class DateTimeToken(
  datetime: DateTime,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = datetime.toString // TODO
  def value = datetime
  def clearLocation: LogicalToken = copy(location = None)
}
object DateTimeToken extends LogicalTokens.SimpleTokenizer {
  def apply(p: DateTime, location: ParseLocation): DateTimeToken =
    DateTimeToken(p, Some(location))

  def apply(config: Config, s: String, location: Option[ParseLocation]): DateTimeToken =
    DateTimeToken(DateTimeUtils.parseIsoDateTimeJst(s), location) // TODO

  def apply(config: Config, s: String, location: ParseLocation): DateTimeToken =
    DateTimeToken(DateTimeUtils.parseIsoDateTimeJst(s), Some(location)) // TODO

  override protected def accept_Token(config: Config, s: String, location: ParseLocation) =
    if (s.count(_ == 'T') == 1 && s.count(_ == ':') >= 2 
      && (
        s.count(_ == '-') match {
          case 2 => s.count(x => x == 'Z' || x == '+') == 1
          case 3 => !s.exists(x => x == 'Z' || x == '+')
          case _ => false
        }
      ))
      Try(apply(config, s, location)).toOption
    else
      None
}

case class LocalDateTimeToken(
  datetime: LocalDateTime,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = datetime.toString // TODO
  def value = datetime
  def clearLocation: LogicalToken = copy(location = None)
}
object LocalDateTimeToken extends LogicalTokens.SimpleTokenizer {
  def apply(p: DateTime, location: ParseLocation): DateTimeToken =
    DateTimeToken(p, Some(location))

  def apply(config: Config, s: String, location: Option[ParseLocation]): LocalDateTimeToken =
    LocalDateTimeToken(LocalDateTimeUtils.parse(s), location)

  def apply(config: Config, s: String, location: ParseLocation): LocalDateTimeToken =
    LocalDateTimeToken(LocalDateTimeUtils.parse(s), Some(location))

  override protected def accept_Token(config: Config, s: String, location: ParseLocation) =
    if (s.count(_ == '-') == 2 && s.count(_ == 'T') == 1 && s.contains(':') &&
      !s.exists(x => x == 'Z' || x == '+'))
      Try(apply(config, s, location)).toOption
    else
      None
}

case class LocalDateToken(
  date: LocalDate,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = date.toString // TODO
  def value = date
  def clearLocation: LogicalToken = copy(location = None)
}
object LocalDateToken extends LogicalTokens.SimpleTokenizer {
  def apply(p: LocalDate, location: ParseLocation): LocalDateToken =
    LocalDateToken(p, Some(location))

  def apply(config: Config, s: String, location: Option[ParseLocation]): LocalDateToken =
    LocalDateToken(LocalDateUtils.parse(s), location)

  def apply(config: Config, s: String, location: ParseLocation): LocalDateToken =
    LocalDateToken(LocalDateUtils.parse(s), Some(location))

  override protected def accept_Token(config: Config, s: String, location: ParseLocation) =
    if (s.count(_ == '-') == 2 && !s.contains(':'))
      Try(apply(config, s, location)).toOption
    else
      None
}

case class LocalTimeToken(
  time: LocalTime,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = time.toString // TODO
  def value = time
  def clearLocation: LogicalToken = copy(location = None)
}
object LocalTimeToken extends LogicalTokens.SimpleTokenizer {
  def apply(p: LocalTime, location: ParseLocation): LocalTimeToken =
    LocalTimeToken(p, Some(location))

  def apply(config: Config, s: String, location: Option[ParseLocation]): LocalTimeToken =
    LocalTimeToken(LocalTime.parse(s), location)

  def apply(config: Config, s: String, location: ParseLocation): LocalTimeToken =
    LocalTimeToken(LocalTime.parse(s), Some(location))

  override protected def accept_Token(config: Config, s: String, location: ParseLocation) =
    if (s.count(_ == ':') <= 2 && !s.contains('-') && !s.contains(','))
      Try(apply(config, s, location)).toOption
    else
      None
}

case class MonthDayToken(
  monthday: MonthDay,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = monthday.toString // TODO
  def value = monthday
  def clearLocation: LogicalToken = copy(location = None)
}
object MonthDayToken extends LogicalTokens.SimpleTokenizer {
  def apply(p: MonthDay, location: ParseLocation): MonthDayToken =
    MonthDayToken(p, Some(location))

  def apply(config: Config, s: String, location: Option[ParseLocation]): MonthDayToken =
    MonthDayToken(MonthDay.parse(s), location)

  def apply(config: Config, s: String, location: ParseLocation): MonthDayToken =
    MonthDayToken(MonthDay.parse(s), Some(location))

  override protected def accept_Token(config: Config, s: String, location: ParseLocation) =
    if (s.startsWith("--") && s.forall(x => StringUtils.isAsciiNumberChar(x) || x == '-'))
      Try(apply(config, s, location)).toOption
    else
      None
}
case class PeriodToken(
  period: Period,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = period.toString // TODO
  def value = period
  def clearLocation: LogicalToken = copy(location = None)
}
object PeriodToken extends LogicalTokens.SimpleTokenizer {
  def apply(p: Period, location: ParseLocation): PeriodToken =
    PeriodToken(p, Some(location))

  def apply(config: Config, s: String, location: Option[ParseLocation]): PeriodToken =
    PeriodToken(Period.parse(s), location)

  def apply(config: Config, s: String, location: ParseLocation): PeriodToken =
    PeriodToken(Period.parse(s), Some(location))

  override protected def accept_Token(config: Config, s: String, location: ParseLocation) =
    None // TODO
}
case class DurationToken(
  duration: Duration,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = duration.toString // TODO
  def value = duration
  def clearLocation: LogicalToken = copy(location = None)
}
object DurationToken extends LogicalTokens.SimpleTokenizer {
  def apply(p: Duration, location: ParseLocation): DurationToken =
    DurationToken(p, Some(location))

  def apply(config: Config, s: String, location: Option[ParseLocation]): DurationToken =
    DurationToken(Duration.parse(s), location)

  def apply(config: Config, s: String, location: ParseLocation): DurationToken =
    DurationToken(Duration.parse(s), Some(location))

  override protected def accept_Token(config: Config, s: String, location: ParseLocation) =
    None // TODO
}
case class DateTimeIntervalToken( // Use IntervalToken if possible
  interval: DateTimePeriod,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = interval.toString // TODO
  def value = interval
  def clearLocation: LogicalToken = copy(location = None)
}
object DateTimeIntervalToken extends LogicalTokens.SimpleTokenizer {
  def apply(p: DateTimePeriod, location: ParseLocation): DateTimeIntervalToken =
    DateTimeIntervalToken(p, Some(location))

  def apply(config: Config, s: String, location: Option[ParseLocation]): DateTimeIntervalToken =
    DateTimeIntervalToken(DateTimePeriod.parse(s), location)

  def apply(config: Config, s: String, location: ParseLocation): DateTimeIntervalToken =
    DateTimeIntervalToken(DateTimePeriod.parse(s), Some(location))

  override protected def accept_Token(config: Config, s: String, location: ParseLocation) =
    if (s.contains("~"))
      DateTimePeriod.parseOption(s).map(apply(_, location)) // TODO timezone, datetime
    else
      None
}

case class UrlToken(
  url: URL,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = url.toString // TODO
  def value = url
  def clearLocation: LogicalToken = copy(location = None)
}
object UrlToken extends LogicalTokens.SimpleTokenizer {
  def apply(s: String, location: Option[ParseLocation]): UrlToken =
    UrlToken(new URL(s), location)

  def apply(s: String, location: ParseLocation): UrlToken =
    UrlToken(new URL(s), Some(location))

  override protected def accept_Token(config: Config, p: String, location: ParseLocation) =
    Try(apply(p, location)).toOption
}

case class UrnToken(
  urn: Urn,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = urn.toString // TODO
  def value = urn
  def clearLocation: LogicalToken = copy(location = None)
}
object UrnToken extends LogicalTokens.SimpleTokenizer {
  def apply(s: String, location: Option[ParseLocation]): UrnToken =
    UrnToken(Urn(s), location)

  def apply(s: String, location: ParseLocation): UrnToken =
    UrnToken(Urn(s), Some(location))

  override protected def accept_Token(config: Config, p: String, location: ParseLocation) =
    if (_is_urn(p))
      Try(apply(p, location)).toOption
    else
      None

  private def _is_urn(p: String) = UriToken.isUri(p) && (Strings.totokens(p, ":") match {
    case Nil => false
    case "urn" :: _ => true
    case _ => false
  })
}

case class UriToken(
  uri: URI,
  location: Option[ParseLocation]
) extends LiteralToken {
  def raw = uri.toString // TODO
  def value = uri
  def clearLocation: LogicalToken = copy(location = None)
}
object UriToken extends LogicalTokens.SimpleTokenizer {
  def apply(s: String, location: Option[ParseLocation]): UriToken =
    UriToken(new URI(s), location)

  def apply(s: String, location: ParseLocation): UriToken =
    UriToken(new URI(s), Some(location))

  override protected def accept_Token(config: Config, p: String, location: ParseLocation) =
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
  def clearLocation: LogicalToken = copy(location = None)
}
object PathToken extends LogicalTokens.SimpleTokenizer {
  def apply(s: String, location: ParseLocation): PathToken =
    PathToken(s, Some(location))

  override protected def accept_Token(config: Config, p: String, location: ParseLocation) =
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
  override def print = text
  def clearLocation: LogicalToken = copy(location = None)
}
object ExpressionToken extends LogicalTokens.SimpleTokenizer {
  def apply(text: String, location: ParseLocation): ExpressionToken =
    ExpressionToken(text, Some(location))

  override protected def accept_Token(config: Config, p: String, location: ParseLocation) =
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
  override def print = text
  def clearLocation: LogicalToken = copy(location = None)
}
object XsvToken extends LogicalTokens.SimpleTokenizer {
  def apply(s: String, location: ParseLocation): XsvToken =
    XsvToken(s, Some(location))

  override protected def accept_Token(config: Config, p: String, location: ParseLocation) =
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
  override def print = text
  def clearLocation: LogicalToken = copy(location = None)
  def lxsv: Lxsv = Lxsv.create(text)
}
object LxsvToken extends LogicalTokens.SimpleTokenizer {
  def apply(s: String, location: ParseLocation): LxsvToken =
    LxsvToken(s, Some(location))

  override protected def accept_Token(config: Config, p: String, location: ParseLocation) =
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
  override def print = text
  def clearLocation: LogicalToken = copy(location = None)
}
object ExplicitLiteralToken {
  def apply(text: String, location: ParseLocation): LiteralToken =
    ExplicitLiteralToken(text, Some(location))
}
