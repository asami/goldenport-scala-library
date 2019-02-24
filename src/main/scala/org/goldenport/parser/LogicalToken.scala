package org.goldenport.parser

import scala.util.Try
import java.net.{URL, URI}
import org.joda.time._
import org.goldenport.Strings
import org.goldenport.values.Urn
import org.goldenport.util.StringUtils
import org.goldenport.util.{DateTimeUtils, LocalDateUtils, LocalDateTimeUtils}
import LogicalTokens.Config

/*
 * @since   Aug. 28, 2018
 *  version Sep. 19, 2018
 *  version Oct. 26, 2018
 *  version Jan.  2, 2019
 * @version Feb. 14, 2019
 * @author  ASAMI, Tomoharu
 */
sealed trait LogicalToken {
  def location: Option[ParseLocation]
  def show: String = ???
}
object LogicalToken {
}

sealed trait LiteralToken extends LogicalToken

trait ExternalLogicalToken extends LiteralToken

case object EndToken extends LogicalToken {
  val location = None
}

case class SpaceToken(
  s: String,
  location: Option[ParseLocation]
) extends LogicalToken {
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
}

case class AtomToken(
  name: String,
  location: Option[ParseLocation]
) extends LiteralToken {
}
object AtomToken {
  def apply(text: String, location: ParseLocation): AtomToken =
    AtomToken(text, Some(location))
}

sealed trait StringToken extends LiteralToken {
  def prefix: Option[String]
  def text: String
}

case class DoubleStringToken(
  text: String,
  location: Option[ParseLocation],
  prefix: Option[String] = None
) extends StringToken {
}
object DoubleStringToken {
  def apply(text: String, location: ParseLocation): DoubleStringToken =
    DoubleStringToken(text, Some(location))
}

case class SingleStringToken(
  text: String,
  location: Option[ParseLocation],
  prefix: Option[String] = None
) extends StringToken {
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
}
object RawStringToken {
  def apply(text: String, location: ParseLocation): RawStringToken =
    RawStringToken(text, Some(location))
}

case class BooleanToken(
  b: Boolean,
  location: Option[ParseLocation]
) extends LiteralToken {
}

case class NumberToken(
  n: BigDecimal,
  location: Option[ParseLocation]
) extends LiteralToken {
}
object NumberToken extends LogicalTokens.SimpleTokenizer {
  def apply(n: Int, location: Option[ParseLocation]): NumberToken =
    NumberToken(BigDecimal(n), location)

  def apply(n: Int, location: ParseLocation): NumberToken =
    NumberToken(BigDecimal(n), Some(location))

  override protected def accept_Token(config: Config, p: String, location: ParseLocation) =
    Try(NumberToken(BigDecimal(p), Some(location))).toOption
}

case class DateTimeToken(
  datetime: DateTime,
  location: Option[ParseLocation]
) extends LiteralToken {
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
}
object LocalTimeToken extends LogicalTokens.SimpleTokenizer {
  def apply(p: LocalTime, location: ParseLocation): LocalTimeToken =
    LocalTimeToken(p, Some(location))

  def apply(config: Config, s: String, location: Option[ParseLocation]): LocalTimeToken =
    LocalTimeToken(LocalTime.parse(s), location)

  def apply(config: Config, s: String, location: ParseLocation): LocalTimeToken =
    LocalTimeToken(LocalTime.parse(s), Some(location))

  override protected def accept_Token(config: Config, s: String, location: ParseLocation) =
    if (s.count(_ == ':') <= 2 && !s.contains('-'))
      Try(apply(config, s, location)).toOption
    else
      None
}

case class MonthDayToken(
  monthday: MonthDay,
  location: Option[ParseLocation]
) extends LiteralToken {
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

case class UrlToken(
  url: URL,
  location: Option[ParseLocation]
) extends LiteralToken {
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

  private def _is_urn(p: String) = Strings.totokens(p, ":") match {
    case Nil => false
    case x :: Nil => false
    case x :: y :: _ => !x.forall(_.isDigit) && !y.startsWith("//") // datetime, url
  }
}

case class PathToken(
  path: String,
  location: Option[ParseLocation]
) extends LiteralToken {
}
object PathToken extends LogicalTokens.SimpleTokenizer {
  def apply(s: String, location: ParseLocation): PathToken =
    PathToken(s, Some(location))

  override protected def accept_Token(config: Config, p: String, location: ParseLocation) =
    if (_is_path(p))
      Try(apply(p, location)).toOption
    else
      None

  private def _is_path(p: String) = p.startsWith("@") || p.contains('/')
}

case class ExpressionToken(
  text: String,
  location: Option[ParseLocation]
) extends LiteralToken {
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
        _is_accept(x) && p.tail.forall(_is_accept)
      ).getOrElse(false)

  private def _is_not_atom(p: String) =
    !(StringUtils.isLispIdentifierI18N(p) || StringUtils.isNumericalSymbol(p))

  // '(', '{', '[' and '@' : in first character, used by another token type.
  // '/' : used by PathToken.
  private def _is_accept(c: Char) = {
    val r = StringUtils.isLispIdentifierI18NChar(c) || StringUtils.isScriptSymbolChar(c)
//    println(s"ExpressionToken#_is_accept $c => $r")
    r
  }
}

case class BracketToken(
  text: String,
  location: Option[ParseLocation],
  prefix: Option[String] = None
) extends LiteralToken {
}
object BracketToken {
  def apply(text: String, location: ParseLocation): BracketToken =
    BracketToken(text, Some(location))
}

case class RawBracketToken(
  text: String,
  location: Option[ParseLocation],
  prefix: Option[String] = None
) extends LiteralToken {
}
object RawBracketToken {
  def apply(text: String, location: ParseLocation): RawBracketToken =
    RawBracketToken(text, Some(location))
}
