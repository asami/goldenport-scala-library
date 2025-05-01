package org.goldenport.test

import scalaz._, Scalaz._
import org.scalatest._
import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.goldenport.test.MatchResultUtils.Implicits._
import org.goldenport.i18n.test._

/*
 * @since   Dec.  8, 2024
 * @version Dec. 26, 2024
 * @author  ASAMI, Tomoharu
 */
trait MatchResultHelperBase extends Matchers {
  protected def match_result(label: String, p: MatchResult, ps: MatchResult*): MatchResult =
    MatchResultUtils.build(label, p +: ps)

  // protected def match_result(label: String)(p: MatchResult): MatchResult =
  //   MatchResultUtils.enlabel(p, label)

  protected def match_result(label: Option[String], p: MatchResult, ps: MatchResult*): MatchResult =
    label.fold(match_result(p, ps: _*))(match_result(_, p, ps: _*))

  protected def match_result(p: MatchResult, ps: MatchResult*): MatchResult =
    (p +: ps.toVector).concatenate

  protected def match_be[T](label: String, expected: T, actual: T): MatchResult =
    match_result(label, be(expected)(actual))

 protected def match_be[T](label: Option[String], expected: T, actual: T): MatchResult =
   match_result(label, be(expected)(actual))

  protected def match_be_option[T](matcher: (T, T) => MatchResult, expected: Option[T], actual: Option[T]): MatchResult =
    MatchResultUtils.matchOption(matcher, expected, actual)

  protected def match_be_option[T](label: String)(matcher: (T, T) => MatchResult, expected: Option[T], actual: Option[T]): MatchResult =
    match_result(label, MatchResultUtils.matchOption(matcher, expected, actual))

  protected def match_be_seq[T](matcher: (T, T) => MatchResult, expected: Seq[T], actual: Seq[T]): MatchResult =
    MatchResultUtils.matchSeq(matcher, expected, actual)

  protected def match_be_map[K, T](matcher: (T, T) => MatchResult, expected: Map[K, T], actual: Map[K, T]): MatchResult =
    MatchResultUtils.matchMap(matcher, expected, actual)

  // protected def match_concatenate(label: String, ms: MatchResult*): MatchResult = {
  //   val mr = ms.toVector.concatenate
  //   MatchResultUtils.enlabel(mr, label)
  // }

  protected final def match_success: MatchResult = MatchResultUtils.success

  protected final def match_error(p: String): MatchResult = MatchResultUtils.error(p)

  protected final def match_error(
    label: String,
    expected: Any,
    actual: Any
  ): MatchResult = MatchResultUtils.makeError(label, expected, actual)

  protected final def match_condition(
    matches: Boolean,
    label: String,
    expected: Any,
    actual: Any
  ): MatchResult = MatchResultUtils.make(
    matches,
    label,
    expected,
    actual
  )
}
