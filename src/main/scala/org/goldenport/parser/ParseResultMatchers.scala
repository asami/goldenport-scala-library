package org.goldenport.parser

import org.scalatest.matchers._

/*
 * @since   Sep. 28, 2020
 * @version Sep. 29, 2020
 * @author  ASAMI, Tomoharu
 */
trait ParseResultMatchers {
  import ParseResultMatchers._

  def parse_object(p: AnyRef) = ObjectMatcher(p)
}

object ParseResultMatchers {
  case class ObjectMatcher(o: AnyRef) extends Matcher[AnyRef] {
    def apply(p: AnyRef) = p match {
      case m: ParseSuccess[_] => MatchResult(m.ast == o, s"${m.ast} was not equal to $o", s"${m.ast} was equal to $o")
      case m: ParseFailure[_] => MatchResult(false, s"Parse failure: $m", s"$p was equal to $o")
      case m: EmptyParseResult[_] => MatchResult(false, s"Parse empty failure: $m", s"$p was equal to $o")
    }
  }
}
