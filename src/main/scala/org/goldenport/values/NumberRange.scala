package org.goldenport.values

import scalaz._, Scalaz._
import scala.util.Try
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.extension.Showable
import org.goldenport.parser._
import org.goldenport.values.RichNumber.Implicits._
import org.goldenport.util.{NumberUtils, AnyRefUtils}

/*
 * @since   Sep. 10, 2019
 *  version Oct. 16, 2019
 *  version Feb. 28, 2020
 *  version Sep. 28, 2020
 * @version Jan. 30, 2021
 * @author  ASAMI, Tomoharu
 */
trait NumberRange extends Showable {
  def display: String = print
  def show: String = print
  def embed: String = print
  def isValid(index: Int): Boolean
  def indexes: List[Int]
  def isMatch(p: Number): Boolean
}

object NumberRange {
  def apply(
    start: Number,
    end: Number,
    startInclusive: Boolean,
    endInclusive: Boolean
  ): RepeatRange = RepeatRange(start, end, startInclusive, endInclusive)

  def create(p: String): NumberRange = parse(p).take

  def parseOption(p: String): Option[NumberRange] = parse(p).toOption

  def parse(s: String): ParseResult[NumberRange] = {
    Strings.totokens(s, ",") match {
      case Nil => ParseResult.success(NoneRange)
      case x :: Nil => _parse(x)
      case xs => _parse(xs)
    }
  }

  private def _parse(ps: List[String]): ParseResult[NumberRange] = {
    // case class Z(
    //   xs: ParseResult[Vector[NumberRange]] = ParseResult.success(Vector.empty),
    //   vs: ParseResult[Vector[ValueRange]] = ParseResult.success(Vector.empty)
    // ) {
    //   def r = if (xs.length == vs.length)
    //     EnumRange(vs.map(_.value))
    //   else
    //     CompositeRange(xs)

    //   def +(rhs: String) = _parse(rhs) match {
    //     case m: ValueRange => Z(xs :+ m, vs :+ m)
    //     case m => Z(xs :+ m)
    //   }
    // }
    case class Z(
      xs: ParseResult[Vector[NumberRange]] = ParseResult.success(Vector.empty)
    ) {
      def r = for {
        ns <- xs
      } yield {
        if (ns.isEmpty) {
          NoneRange
        } else {
          val vs = ns.collect {
            case m: ValueRange => m
          }
          if (ns.length == vs.length)
            EnumRange(vs.map(_.value))
          else
            CompositeRange(ns)
        }
      }

      def +(rhs: String) = {
        val z = for {
          x <- xs
          a <- _parse(rhs)
        } yield x :+ a
        copy(xs = z)
      }
    }
    ps./:(Z())(_+_).r
  }

  private def _parse(p: String): ParseResult[NumberRange] =
    if (p.contains("~"))
      RepeatRange.parse(p)
    else
      ValueRange.parse(p)

  def parseLabel(labelf: String => Option[Number], p: String): ParseResult[NumberRange] = {
    Strings.totokens(p, ",") match {
      case Nil => ParseResult.success(NoneRange)
      case x :: Nil => _parse(labelf, x)
      case xs => _parse(labelf, xs)
    }
  }

  private def _parse(labelf: String => Option[Number], ps: List[String]): ParseResult[NumberRange] = {
    case class Z(xs: Vector[ParseResult[NumberRange]] = Vector.empty, vs: Vector[ParseResult[ValueRange]] = Vector.empty) {
      def r: ParseResult[NumberRange] = if (xs.length == vs.length)
        vs.sequence.map(x => EnumRange(x.map(_.value)))
      else
        xs.sequence.map(CompositeRange(_))

      def +(rhs: String) = _parse(labelf: String => Option[Number], rhs) match {
        case m: ParseSuccess[_] => m.ast match {
          case _: ValueRange => Z(xs :+ m, vs :+ m.asInstanceOf[ParseResult[ValueRange]])
          case _ => Z(xs :+ m.asInstanceOf[ParseResult[NumberRange]])
        }
        case m: ParseFailure[_] => Z(xs :+ m)
        case m: EmptyParseResult[_] => this
      }
    }
    ps./:(Z())(_+_).r
  }

  private def _parse(labelf: String => Option[Number], p: String): ParseResult[NumberRange] =
    if (p.contains("~"))
      RepeatRange.parse(labelf, p)
    else
      ValueRange.parse(labelf, p)

  def createInt(ps: Seq[Int]): NumberRange = EnumRange.createInt(ps)
}

case object NoneRange extends NumberRange {
  def print: String = "none"
  def isValid(index: Int): Boolean = false
  def indexes: List[Int] = Nil
  def isMatch(p: Number): Boolean = false
}

case class CompositeRange(ranges: Seq[NumberRange]) extends NumberRange {
  def print: String = ranges.map(_.print).mkString(",")
  def isValid(index: Int): Boolean = ranges.exists(_.isValid(index))
  def indexes: List[Int] = ranges.toList.flatMap(_.indexes)
  def isMatch(p: Number): Boolean = ranges.exists(_.isMatch(p))
}
object CompositeRange {
  def apply(p: NumberRange, ps: NumberRange*): CompositeRange =
    CompositeRange(p +: ps)
}

case class ValueRange(value: Number) extends NumberRange {
  def print: String = value.toString
  def isValid(index: Int): Boolean = value.intValue == index
  def indexes: List[Int] = List(value.intValue)
  def isMatch(p: Number): Boolean = p == value
}
object ValueRange {
  def parse(p: String): ParseResult[ValueRange] = ParseResult(ValueRange(AnyRefUtils.toNumber(p)))

  def parse(labelf: String => Option[Number], p: String): ParseResult[ValueRange] =
    for {
      a <- NumberUtils.parse(labelf, p)
    } yield ValueRange(a)
}

case class EnumRange(ranges: Seq[Number]) extends NumberRange {
  private lazy val _ints = ranges.map(_.intValue)

  def print: String = ranges.mkString(",")
  def isValid(index: Int): Boolean = _ints.contains(index)
  def indexes: List[Int] = ranges.toList.map(_.intValue)
  def isMatch(p: Number): Boolean = ranges.contains(p)
}
object EnumRange {
  def apply(p: Number, ps: Number*): EnumRange = EnumRange(p +: ps.toVector)
  def createInt(ps: Seq[Int]): EnumRange = EnumRange(ps.map(_.asInstanceOf[Number]))
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

  def indexes: List[Int] = _ints.toList

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

  def isMatch(p: Number): Boolean =
    if (step == 1) {
      p match {
        case m: java.lang.Integer => _is_match_interval(m)
        case m: java.lang.Long => _is_match_interval(m)
        case m: java.lang.Short => _is_match_interval(m)
        case m: java.lang.Byte => _is_match_interval(m)
        case m => _is_match(m)
      }
    } else {
      _is_match(p)
    }

  private def _is_match_interval(p: Number): Boolean = {
    val l = if (startInclusive)
      start <= p
    else
      start < p
    val h = if (endInclusive)
      p <= end
    else
      p < end
    l && h
  }

  private def _is_match(p: Number): Boolean = {
    @annotation.tailrec
    def go(n: Number): Boolean = {
      if (p == n)
        true
      if (_is_out(n))
        false
      else
        go(_next(n))
    }
    if (startInclusive)
      go(start)
    else
      go(_next(start))
  }

  private def _next(p: Number): Number = stepOperation match {
    case RepeatRange.PlusStep => p + step
    case RepeatRange.MinusStep => p - step
  }

  private def _is_out(p: Number): Boolean = stepOperation match {
    case RepeatRange.PlusStep =>
      if (endInclusive)
        end < p
      else
        end <= p
    case RepeatRange.MinusStep =>
      if (startInclusive)
        p < start
      else
        p <= start
  }

}
object RepeatRange {
  import IntervalFactory.{parsePrefix, parseStartPostfix, parsePostfix}

  sealed trait StepOperation {
    def mark: String
  }
  case object PlusStep extends StepOperation {
    def mark = "+"
  }
  case object MinusStep extends StepOperation {
    def mark = "-"
  }

  private val _regex = """([\[\(])?([^~^!]+)(!)?~([^+^-^!]+)([!)\]])?([+-])?(.+)?""".r

  def apply(
    start: Number,
    end: Number
  ): RepeatRange = apply(start, end, true, true)

  def apply(
    start: Number,
    end: Number,
    startInclusive: Boolean,
    endInclusive: Boolean
  ): RepeatRange = RepeatRange(
    start, end, 1, PlusStep, startInclusive, endInclusive
  )

  def parse(p: String): ParseResult[RepeatRange] = parseCloseClose(p)

  def parseCloseClose(p: String): ParseResult[RepeatRange] = ParseResult.parse {
    val _regex(prefix, start, spostfix, end, postfix, sop, step) = p
    _parse(prefix, start, spostfix, end, postfix, sop, step, true)
  }

  def parseCloseOpen(p: String): ParseResult[RepeatRange] = ParseResult.parse {
    val _regex(prefix, start, spostfix, end, postfix, sop, step) = p
    _parse(prefix, start, spostfix, end, postfix, sop, step, false)
  }

  private def _parse(
    prefix: String,
    start: String,
    spostfix: String,
    end: String,
    postfix: String,
    sop: String,
    step: String,
    endincludedefault: Boolean
  ): ParseResult[RepeatRange] = {
    for {
      s <- NumberUtils.parse(start)
      e <- NumberUtils.parse(end)
      sinc <- Option(prefix).map(parsePrefix).orElse(Option(spostfix).map(parseStartPostfix)).sequence
      einc <- Option(postfix).map(parsePostfix).sequence
      sp <- Option(step).map(NumberUtils.parse).sequence
      r <- _parse(
        s,
        sinc,
        e,
        einc,
        Option(sop),
        sp,
        endincludedefault
      )
    } yield r
  }

  private def _parse(
    start: Number,
    sinc: Option[Boolean],
    end: Number,
    einc: Option[Boolean],
    pop: Option[String],
    pstep: Option[Number],
    endincludedefault: Boolean
  ): ParseResult[RepeatRange] = _parse(
    start,
    sinc getOrElse true,
    end,
    einc getOrElse endincludedefault,
    pop,
    pstep
  )

  private def _parse(
    start: Number,
    sinc: Boolean,
    end: Number,
    einc: Boolean,
    pop: Option[String],
    pstep: Option[Number]
  ): ParseResult[RepeatRange] = ParseResult {
    val step: Number = pstep getOrElse 1
    val op = pop.map {
      case "+" => PlusStep
      case "-" => MinusStep
      case m => RAISE.invalidArgumentFault(s"Invalid operation: $m")
    }.getOrElse(PlusStep)
    RepeatRange(start, end, step, op, sinc, einc)
  }

  def parse(labelf: String => Option[Number], p: String): ParseResult[RepeatRange] =
    parseCloseClose(labelf, p)

  def parseCloseClose(labelf: String => Option[Number], p: String): ParseResult[RepeatRange] =
    _parse(labelf, p, true)

  def parseCloseOpen(labelf: String => Option[Number], p: String): ParseResult[RepeatRange] =
    _parse(labelf, p, false)

  private def _parse(labelf: String => Option[Number], p: String, endincludedefault: Boolean): ParseResult[RepeatRange] = ParseResult.parse {
    val _regex(prefix, start, spostfix, end, postfix, sop, step) = p
    for {
      s <- NumberUtils.parse(labelf, start)
      e <- NumberUtils.parse(labelf, end)
      sinc <- Option(prefix).map(parsePrefix).orElse(Option(spostfix).map(parseStartPostfix)).sequence
      einc <- Option(postfix).map(parsePostfix).sequence
      r <- _parse(
        s,
        sinc,
        e,
        einc,
        Option(sop),
        Option(step).map(AnyRefUtils.toNumber),
        endincludedefault
      )
    } yield r
  }
}
