package org.goldenport.test

import org.scalatest.matchers.{BeMatcher, MatchResult}

/*
 * @since   Dec.  3, 2024
 * @version Dec. 23, 2024
 * @author  ASAMI, Tomoharu
 */
case class OptionBeMatcher[T](
  label: Option[String],
  matcher: (T, T) => MatchResult
)(right: Option[T]) extends BeMatcher[Option[T]] {
  def apply(left: Option[T]): MatchResult = ???
}

object OptionBeMatcher {
  def apply[T](label: String, matcher: (T, T) => MatchResult)(expected: Option[T]): OptionBeMatcher[T] =
    OptionBeMatcher(Some(label), matcher)(expected)
}
