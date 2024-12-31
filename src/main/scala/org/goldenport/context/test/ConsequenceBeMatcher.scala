package org.goldenport.context.test

import scalaz._, Scalaz._
import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.goldenport.context._


/*
 * @since   Dec.  2, 2024
 * @version Dec.  2, 2024
 * @author  ASAMI, Tomoharu
 */
case class ConsequenceBeMatcher[T](matcher: BeMatcher[T]) extends BeMatcher[Consequence[T]] {
  def apply(left: Consequence[T]): MatchResult = left match {
    case Consequence.Success(s, _) => matcher(s)
    case Consequence.Error(c) => MatchResult(false, c.message, "Success")
  }
}
