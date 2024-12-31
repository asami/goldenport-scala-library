package org.goldenport.test

import scalaz._, Scalaz._
import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.goldenport.util.AnyUtils

/*
 * @since   Dec.  7, 2024
 * @version Dec. 26, 2024
 * @author  ASAMI, Tomoharu
 */
object MatchResultUtils {
  val success = MatchResult(true, "Success", "Success")

  object Implicits {
    implicit object MatchResultMonoid extends Monoid[MatchResult] {
      val zero = success
      def append(lhs: MatchResult, rhs: => MatchResult) = MatchResultUtils.append(lhs, rhs)
    }
  }

  def build(label: String, p: MatchResult, ps: MatchResult*): MatchResult =
    enlabel(concatnate(p +: ps), label)

  def build(label: String, ps: Seq[MatchResult]): MatchResult =
    enlabel(concatnate(ps), label)

  def concatnate(p: MatchResult, ps: MatchResult*): MatchResult =
    concatnate(p +: ps)

  def concatnate(ps: Seq[MatchResult]): MatchResult = {
    import Implicits._
    ps.toVector.concatenate
  }

  def append(lhs: MatchResult, rhs: MatchResult) =
    (lhs.matches, rhs.matches) match {
      case (true, true) => lhs
      case (true, false) => rhs
      case (false, true) => lhs
      case (false, false) => _concat_messages(lhs, rhs)
    }

  private def _concat_messages(lhs: MatchResult, rhs: MatchResult) = {
    def _concat_(l: String, r: String) = l + ":" + r
    lhs.copy(
      rawFailureMessage = _concat_(lhs.rawFailureMessage, rhs.rawFailureMessage),
      rawNegatedFailureMessage = _concat_(lhs.rawNegatedFailureMessage, rhs.rawNegatedFailureMessage),
      rawMidSentenceFailureMessage = _concat_(lhs.rawMidSentenceFailureMessage, rhs.rawMidSentenceFailureMessage),
      rawMidSentenceNegatedFailureMessage = _concat_(lhs.rawMidSentenceNegatedFailureMessage, rhs.rawMidSentenceNegatedFailureMessage),
      failureMessageArgs = lhs.failureMessageArgs ++ rhs.failureMessageArgs,
      negatedFailureMessageArgs = lhs.negatedFailureMessageArgs ++ rhs.negatedFailureMessageArgs,
      midSentenceFailureMessageArgs = lhs.midSentenceFailureMessageArgs ++ rhs.midSentenceFailureMessageArgs,
      midSentenceNegatedFailureMessageArgs = lhs.midSentenceNegatedFailureMessageArgs ++ rhs.midSentenceNegatedFailureMessageArgs
    )
  }

  def enlabel(p: MatchResult, label: String): MatchResult = {
    def _label_(p: String) = s"[$label]$p"

    if (p.matches)
      p
    else
      MatchResult(
        false,
        _label_(p.rawFailureMessage),
        _label_(p.rawNegatedFailureMessage),
        _label_(p.rawMidSentenceFailureMessage),
        _label_(p.rawMidSentenceNegatedFailureMessage),
        p.failureMessageArgs,
        p.negatedFailureMessageArgs,
        p.midSentenceFailureMessageArgs,
        p.midSentenceNegatedFailureMessageArgs,
        p.prettifier
      )
  }

  def matchOption[T](matcher: (T, T) => MatchResult, expected: Option[T], actual: Option[T]): MatchResult =
    (expected, actual) match {
      case (Some(e), Some(a)) => matcher(e, a)
      case (Some(e), None) => MatchResult(false, "Actual is None", "Actual is not None")
      case (None, Some(a)) => MatchResult(false, "Actual is not None", "Actual is None")
      case (None, None) => success
    }

  def matchSeq[T](matcher: (T, T) => MatchResult, expected: Seq[T], actual: Seq[T]): MatchResult = {
    if (expected.length != actual.length) {
      error(s"Exptected length ${expected.length} does not match actural length ${actual.length}")
    } else {
      import Implicits._

      val xs = expected zip actual
      xs.toVector.foldMap {
        case (e, a) => matcher(e, a)
      }
    }
  }

  def matchMap[K, T](matcher: (T, T) => MatchResult, expected: Map[K, T], actual: Map[K, T]): MatchResult = {
    if (expected.size != actual.size) {
      error(s"Exptected size ${expected.size} does not match actural size ${actual.size}")
    } else {
      import Implicits._

      expected.toVector.foldMap {
        case (k, v) => ???
      }
    }
  }

  def error(msg: String) = MatchResult(false, msg, msg)

  def make(
    matches: Boolean,
    label: String,
    expected: Any,
    actual: Any
  ): MatchResult = {
    val se = AnyUtils.toString(expected)
    val sa = AnyUtils.toString(actual)
    val m = s"$label: '$sa' does not equal '$se'."
    val mn = s"$label: $sa equals '$se', but i shouldn't be."
    MatchResult(matches, m, mn)
  }

  def makeError(
    label: String,
    expected: Any,
    actual: Any
  ) = make(false, label, expected, actual)
}
