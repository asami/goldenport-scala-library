package org.goldenport.values

import scala.util.Try
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.extension.Showable
import org.goldenport.util.AnyRefUtils

/*
 * @since   Sep. 10, 2019
 * @version Sep. 15, 2019
 * @author  ASAMI, Tomoharu
 */
trait NumberRange extends Showable {
  def display: String = print
  def show: String = print
  def isValid(index: Int): Boolean
}

object NumberRange {
  def parseOption(p: String): Option[NumberRange] = Try(parse(p)).toOption

  def parse(s: String): NumberRange = {
    Strings.totokens(s, ",") match {
      case Nil => NoneRange
      case x :: Nil => _parse(x)
      case xs => _parse(xs)
    }
  }

  private def _parse(ps: List[String]): NumberRange = {
    case class Z(xs: Vector[NumberRange] = Vector.empty, vs: Vector[ValueRange] = Vector.empty) {
      def r = if (xs.length == vs.length)
        EnumRange(vs.map(_.value))
      else
        CompositeRange(xs)

      def +(rhs: String) = _parse(rhs) match {
        case m: ValueRange => Z(xs :+ m, vs :+ m)
        case m => Z(vs :+ m)
      }
    }
    ps./:(Z())(_+_).r
  }

  private def _parse(p: String): NumberRange =
    if (p.contains("~"))
      RepeatRange.parse(p)
    else
      ValueRange.parse(p)
}

case object NoneRange extends NumberRange {
  def print: String = "none"
  def isValid(index: Int): Boolean = false
}

case class CompositeRange(ranges: Seq[NumberRange]) extends NumberRange {
  def print: String = ranges.map(_.print).mkString(",")
  def isValid(index: Int): Boolean = ranges.exists(_.isValid(index))
}

case class ValueRange(value: Number) extends NumberRange {
  def print: String = value.toString
  def isValid(index: Int): Boolean = value.intValue == index
}
object ValueRange {
  def parse(p: String): ValueRange = ValueRange(AnyRefUtils.toNumber(p))
}

case class EnumRange(ranges: Seq[Number]) extends NumberRange {
  private lazy val _ints = ranges.map(_.intValue)

  def print: String = ranges.mkString(",")
  def isValid(index: Int): Boolean = _ints.contains(index)
}

case class RepeatRange(
  start: Number,
  end: Number,
  step: Number,
  stepOperation: RepeatRange.StepOperation,
  startInclusive: Boolean,
  endInclusive: Boolean
) extends NumberRange {
  private lazy val _start_int = start.intValue
  private lazy val _end_int = end.intValue
  private lazy val _step_int = step.intValue

  private def _ints: Stream[Int] =
    if (startInclusive)
      _go_ints(start.intValue, Some(start.intValue))
    else
      _go_ints(start.intValue, None)

  @annotation.tailrec
  private def _go_ints(p: Int, prepend: Option[Int]): Stream[Int] = {
    val v = p + _step_int
    if (v == _end_int) {
      if (endInclusive)
        prepend.map(_ #:: Stream(v)).getOrElse(Stream(v))
      else
        prepend.map(_ #:: Stream.empty).getOrElse(Stream.empty)
    } else if (v > _end_int) {
      prepend.map(_ #:: Stream.empty).getOrElse(Stream.empty)
    } else {
      _go_ints(v, Some(v))
    }
  }

  def print: String = {
    val startmark = if (startInclusive) "" else "!"
    val endmark = if (endInclusive) "" else "!"
    val op = stepOperation.mark
    s"$start$startmark~$end$endmark$op$step"
  }

  def isValid(index: Int): Boolean =
    _ints.contains(index)
}
object RepeatRange {
  sealed trait StepOperation {
    def mark: String
  }
  case object PlusStep extends StepOperation {
    def mark = "+"
  }
  case object MinusStep extends StepOperation {
    def mark = "-"
  }

  private val _regex = """([^~^!]+)(!)?~([^+^-^!]+)(!)?([+-])?(.+)?""".r

  def apply(
    start: Number,
    end: Number,
    startInclusive: Boolean,
    endInclusive: Boolean
  ): RepeatRange = RepeatRange(
    start, end, 1, PlusStep, startInclusive, endInclusive
  )

  def parse(p: String): RepeatRange = {
    val _regex(start, sinc, end, einc, pm, step) = p
    _parse(
      AnyRefUtils.toNumber(start),
      Option(sinc).isEmpty,
      AnyRefUtils.toNumber(end),
      Option(einc).isEmpty,
      Option(pm),
      Option(step).map(AnyRefUtils.toNumber)
    )
  }

  private def _parse(start: Number, sinc: Boolean, end: Number, einc: Boolean, pop: Option[String], pstep: Option[Number]) = {
    val step: Number = pstep getOrElse 1
    val op = pop.map {
      case "+" => PlusStep
      case "-" => MinusStep
      case m => RAISE.invalidArgumentFault(s"Invalid operation: $m")
    }.getOrElse(PlusStep)
    RepeatRange(start, end, step, op, sinc, einc)
  }
}
