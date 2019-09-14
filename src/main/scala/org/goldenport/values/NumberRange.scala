package org.goldenport.values

import scala.util.Try
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.util.AnyRefUtils

/*
 * @since   Sep. 10, 2019
 * @version Sep. 12, 2019
 * @author  ASAMI, Tomoharu
 */
trait NumberRange {
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
        CompositeRange(vs)

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
}

case class CompositeRange(ranges: Seq[NumberRange]) extends NumberRange {
}

case class ValueRange(value: Number) extends NumberRange {
}
object ValueRange {
  def parse(p: String): ValueRange = ValueRange(AnyRefUtils.toNumber(p))
}

case class EnumRange(ranges: Seq[Number]) extends NumberRange {
}

case class RepeatRange(
  start: Number,
  end: Number,
  step: Number,
  stepOperation: RepeatRange.StepOperation,
  startInclusive: Boolean,
  endInclusive: Boolean
) extends NumberRange {
}
object RepeatRange {
  sealed trait StepOperation
  case object PlusStep extends StepOperation
  case object MinusStep extends StepOperation

  private val _regex = """([^~^!]+)(!)?~([^+^-^!]+)(!)?([+-])?(.+)?""".r

  def parse(p: String): RepeatRange = {
    val _regex(start, sinc, end, einc, pm, step) = p
    _parse(
      AnyRefUtils.toNumber(start),
      Option(sinc).isDefined,
      AnyRefUtils.toNumber(end),
      Option(einc).isDefined,
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
