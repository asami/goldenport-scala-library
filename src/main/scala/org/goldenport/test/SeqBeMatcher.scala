package org.goldenport.test

import org.scalatest.matchers.{BeMatcher, MatchResult}

/*
 * @since   Dec. 23, 2024
 * @version Dec. 25, 2024
 * @author  ASAMI, Tomoharu
 */
case class SeqBeMatcher[T](label: Option[String], matcher: (T, T) => MatchResult)(expected: Seq[T]) extends BeMatcher[Seq[T]] with MatchResultHelper {
  def apply(actual: Seq[T]): MatchResult = {
    val r = MatchResultUtils.matchSeq(matcher, expected, actual)
    match_result(label, r)
  }
}

object SeqBeMatcher {
  def apply[T](label: String, matcher: (T, T) => MatchResult)(expected: Seq[T]): SeqBeMatcher[T] =
    SeqBeMatcher(Some(label), matcher)(expected)
}
