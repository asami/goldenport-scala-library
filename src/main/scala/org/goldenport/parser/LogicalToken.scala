package org.goldenport.parser

import scala.util.Try
import java.net.{URL, URI}
import org.joda.time._
import org.goldenport.Strings
import org.goldenport.values.Urn
import org.goldenport.util.{DateTimeUtils, LocalDateUtils}
import LogicalTokens.Config

/*
 * @since   Aug. 28, 2018
 * @version Sep. 19, 2018
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
) extends LiteralToken {
}
object DelimiterToken {
  def apply(c: Char): DelimiterToken = DelimiterToken(c.toString, None)

  def apply(c: Char, location: ParseLocation): DelimiterToken = DelimiterToken(c.toString, Some(location))

  def apply(text: String, location: ParseLocation): DelimiterToken =
    DelimiterToken(text, Some(location))
}

sealed trait StringToken extends LiteralToken {
  def prefix: Option[String]
  def text: String
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
    Try(apply(config, s, location)).toOption
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

  override protected def accept_Token(config: Config, p: String, location: ParseLocation) =
    Try(apply(config, p, location)).toOption
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
    Try(apply(config, s, location)).toOption
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

  private def _is_path(p: String) = p.contains('/')
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
