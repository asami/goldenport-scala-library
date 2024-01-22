package org.goldenport.values

import scalaz._, Scalaz._
import spire.math.Interval
import spire.math.interval._
import org.goldenport.extension.Showable
import org.goldenport.parser.{ParseResult, ParseSuccess, ParseFailure}
import org.goldenport.util.AnyRefUtils

/*
 * @since   Sep.  5, 2020
 *  version Sep. 29, 2020
 *  version Oct. 12, 2020
 *  version Jan. 20, 2021
 * @version Mar. 19, 2022
 * @author  ASAMI, Tomoharu
 */
trait IntervalBase[E] extends Showable {
  def interval: Interval[E]

  def print = {
    val start = _to_bound_start(interval.lowerBound)
    val end = _to_bound_end(interval.upperBound)
    s"$start~$end"
  }
  def display = print
  def show = print

  private def _to_bound_start(p: Bound[E]) = p match {
    case Open(a) => s"(${AnyRefUtils.toString(a)}"
    case Closed(a) => s"[${AnyRefUtils.toString(a)}"
    case Unbound() => ""
    case EmptyBound() => ""
  }

  private def _to_bound_end(p: Bound[E]) = p match {
    case Open(a) => s"${AnyRefUtils.toString(a)})"
    case Closed(a) => s"${AnyRefUtils.toString(a)}]"
    case Unbound() => ""
    case EmptyBound() => ""
  }
}

trait IntervalFactory[T <: IntervalBase[E], E] {
  import IntervalFactory._

  implicit def _order: spire.algebra.Order[E]

  protected def to_Value(p: String): ParseResult[E]
  protected def to_Interval(p: Interval[E]): ParseResult[T]

  def take(p: String): T = parse(p).take

  def parseOption(p: String): Option[T] = parse(p).toOption

  def parse(p: String): ParseResult[T] = parse(CloseClose, p)

  def parse(bounds: BoundsKind, p: String): ParseResult[T] =
    for {
      rr <- _regex(p)
      l <- rr.start.map(to_Value).sequence
      r <- rr.end.map(to_Value).sequence
      linc = rr.startinclude getOrElse bounds.isStartInclusive
      rinc = rr.endinclude getOrElse bounds.isEndInclusive
      x = _parse(l, linc, r, rinc)
      r <- to_Interval(x)
    } yield r

  private def _regex(p: String): ParseResult[RegexResult] = try {
    val regex(prefix, start, spostfix, end, postfix) = p
    for {
      sinc <- Option(prefix).map(parsePrefix).orElse(Option(spostfix).map(parseStartPostfix)).sequence
      einc <- Option(postfix).map(parsePostfix).sequence
    } yield RegexResult(Option(start), Option(end), sinc, einc)
  } catch {
    case m: MatchError => ParseResult.error(s"No interval: $p")
  }

  // private def _prefix(p: String): ParseResult[Boolean] = p match {
  //   case MARK_START_OPEN => ParseResult.success(false)
  //   case MARK_START_CLOSE => ParseResult.success(true)
  //   case m => ParseFailure(s"Unknown start prefix: $m")
  // }

  // private def _start_postfix(p: String): ParseResult[Boolean] = p match {
  //   case MARK_OPEN => ParseResult.success(false)
  //   case m => ParseFailure(s"Unknown start postfix: $m")
  // }

  // private def _postfix(p: String): ParseResult[Boolean] = p match {
  //   case MARK_END_OPEN => ParseResult.success(false)
  //   case MARK_END_CLOSE => ParseResult.success(true)
  //   case MARK_OPEN => ParseResult.success(false)
  //   case m => ParseFailure(s"Unknown end postfix: $m")
  // }

  // private def _is_close(p: String): ParseResult[Boolean] = p match {
  //   case MARK_OPEN => ParseResult(false)
  //   case MARK_CLOSE => ParseResult(true)
  //   case m => ParseResult.error(s"Unknown mark: $m")
  // }

  private def _parse(
    start: Option[E],
    sinc: Boolean,
    end: Option[E],
    einc: Boolean
  ): Interval[E] = {
    val low: Bound[E] = start.map(_to_b(_, sinc)).getOrElse(Unbound())
    val high: Bound[E] = end.map(_to_b(_, einc)).getOrElse(Unbound())
    Interval.fromBounds(low, high)
  }

  private def _to_b(p: E, inclusive: Boolean): Bound[E] =
    if (inclusive)
      Closed(p)
    else
      Open(p)
}

object IntervalFactory {
  val MARK_OPEN = "!" // compatibility
  val MARK_START_OPEN = "("
  val MARK_END_OPEN = ")"
  val MARK_START_CLOSE = "["
  val MARK_END_CLOSE = "]"

  val regex = """([\[\(])?([^~^!]+)?(!)?~([^+^-^!^)^\]]+)?([!)\]])?""".r

  sealed trait BoundsKind {
    def isStartInclusive: Boolean
    def isEndInclusive: Boolean
  }
  case object OpenOpen extends BoundsKind {
    def isStartInclusive: Boolean = false
    def isEndInclusive: Boolean = false
  }
  case object OpenClose extends BoundsKind {
    def isStartInclusive: Boolean = false
    def isEndInclusive: Boolean = true
  }
  case object CloseOpen extends BoundsKind {
    def isStartInclusive: Boolean = true
    def isEndInclusive: Boolean = false

  }
  case object CloseClose extends BoundsKind {
    def isStartInclusive: Boolean = true
    def isEndInclusive: Boolean = true
  }

  case class RegexResult(
    start: Option[String],
    end: Option[String],
    startinclude: Option[Boolean],
    endinclude: Option[Boolean]
  )

  def parsePrefix(p: String): ParseResult[Boolean] = p match {
    case MARK_START_OPEN => ParseResult.success(false)
    case MARK_START_CLOSE => ParseResult.success(true)
    case m => ParseFailure(s"Unknown start prefix: $m")
  }

  def parseStartPostfix(p: String): ParseResult[Boolean] = p match {
    case MARK_OPEN => ParseResult.success(false)
    case m => ParseFailure(s"Unknown start postfix: $m")
  }

  def parsePostfix(p: String): ParseResult[Boolean] = p match {
    case MARK_END_OPEN => ParseResult.success(false)
    case MARK_END_CLOSE => ParseResult.success(true)
    case MARK_OPEN => ParseResult.success(false)
    case m => ParseFailure(s"Unknown end postfix: $m")
  }
}
