package org.goldenport.parser

import org.joda.time._

/*
 * @since   Aug. 28, 2018
 * @version Sep.  2, 2018
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

sealed trait  StringToken extends LiteralToken {
  def text: String
}

case class AtomToken(
  s: String,
  location: Option[ParseLocation]
) extends LiteralToken {
}
object AtomToken {
  def apply(text: String, location: ParseLocation): AtomToken =
    AtomToken(text, Some(location))
}

case class DoubleStringToken(
  text: String,
  location: Option[ParseLocation]
) extends StringToken {
}
object DoubleStringToken {
  def apply(text: String, location: ParseLocation): DoubleStringToken =
    DoubleStringToken(text, Some(location))
}

case class SingleStringToken(
  text: String,
  location: Option[ParseLocation]
) extends StringToken {
}
object SingleStringToken {
  def apply(text: String, location: ParseLocation): SingleStringToken =
    SingleStringToken(text, Some(location))
}

case class RawStringToken(
  text: String,
  location: Option[ParseLocation]
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

case class DateTimeToken(
  dt: DateTime,
  location: Option[ParseLocation]
) extends LiteralToken {
}

case class LocalDateToken(
  d: LocalDate,
  location: Option[ParseLocation]
) extends LiteralToken {
}

case class LocalTimeToken(
  t: LocalTime,
  location: Option[ParseLocation]
) extends LiteralToken {
}
