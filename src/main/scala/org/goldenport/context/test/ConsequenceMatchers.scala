package org.goldenport.context.test

import org.scalatest.matchers.{Matcher, MatchResult}
import org.goldenport.context._

/*
 * @since   May. 11, 2025
 * @version May. 11, 2025
 * @author  ASAMI, Tomoharu
 */
trait ConsequenceMatchers {
  protected def be_success[T](expected: T): Matcher[Consequence[T]] =
    Matcher {
      case Consequence.Success(actual, _) =>
        MatchResult(
          actual == expected,
          s"Success contained unexpected result: $actual",
          s"Success contained expected result: $expected"
        )
      case Consequence.Error(c) =>
        MatchResult(
          matches = false,
          s"Expected Success($expected) but got Error(${c.message})",
          s"Unexpectedly matched Success($expected)"
        )
    }
}
