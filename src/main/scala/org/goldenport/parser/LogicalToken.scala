package org.goldenport.parser

/*
 * @since   Aug. 28, 2018
 * @version Aug. 29, 2018
 * @author  ASAMI, Tomoharu
 */
sealed trait LogicalToken {
}
object LogicalToken {
}

trait ExternalLogicalToken extends LogicalToken

sealed trait  StringToken extends LogicalToken {
  def text: String
}

case class AtomToken(s: String) extends LogicalToken {
}

case class SpaceToken(s: String) extends LogicalToken {
}

case class DelimiterToken(s: String) extends LogicalToken {
}
object DelimiterToken {
  def apply(c: Char): DelimiterToken = DelimiterToken(c.toString)
}

case class DoubleStringToken(text: String) extends StringToken {
}

case class SingleStringToken(text: String) extends StringToken {
}

case class RawStringToken(text: String) extends StringToken {
}
