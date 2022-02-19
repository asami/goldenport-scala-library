package org.goldenport.values

import scalaz._, Scalaz._
import scala.util.Try
import scala.util.control.NonFatal
import java.util.Date
import java.text.SimpleDateFormat
import java.sql.Timestamp
import com.github.nscala_time.time.Imports._
import org.joda.time.LocalDateTime
import org.joda.time.DateTimeConstants._
import spire.math.Interval
import spire.math.interval._
import org.goldenport.RAISE
import org.goldenport.parser.ParseResult
import org.goldenport.context.DateTimeContext
import org.goldenport.util.{DateTimeUtils, DateUtils, AnyUtils}
import org.goldenport.util.LocalDateTimeUtils
import org.goldenport.util.SpireUtils.Implicits._
import org.goldenport.values.Week._
import org.goldenport.values.IntervalFactory.{BoundsKind, CloseOpen, CloseClose}

/*
 * Derived from DateTimePeriod.
 * 
 * @since   Jan. 20, 2021
 *  version Jan. 23, 2021
 *  version Feb. 28, 2021
 * @version Feb. 17, 2022
 * @author  ASAMI, Tomoharu
 */
case class LocalDateTimeInterval( // TODO DateTimeInterval (java8 time)
  start: Option[LocalDateTime],
  end: Option[LocalDateTime],
  isStartInclusive: Boolean = true,
  isEndInclusive: Boolean = true
) {
  import LocalDateTimeInterval._

  def toInterval: Interval[LocalDateTime] = {
    val lower = _to_bound(start, isStartInclusive)
    val higher = _to_bound(end, isEndInclusive)
    Interval.fromBounds(lower, higher)
  }

  private def _to_bound(p: Option[LocalDateTime], inclusive: Boolean): Bound[LocalDateTime] =
    p.map(x => if (inclusive) Closed(x) else Open(x)).getOrElse(Unbound())

  def toDateTimePeriod(tz: DateTimeZone): DateTimePeriod = {
    val s = start.map(_.toDateTime(tz))
    val e = end.map(_.toDateTime(tz))
    DateTimePeriod(s, e, isStartInclusive, isEndInclusive)
  }

  def startLocalDate: Option[LocalDate] = start.map(_.toLocalDate)

  def endLocalDate: Option[LocalDate] = end.map(_.toLocalDate)

  def plusYears(n: Int): LocalDateTimeInterval = {
    copy(start = start.map(_.plusYears(n)), end = end.map(_.plusYears(n)))
  }

  def plusMonths(n: Int): LocalDateTimeInterval = {
    copy(start = start.map(_.plusMonths(n)), end = end.map(_.plusMonths(n)))
  }

  def plusDays(n: Int): LocalDateTimeInterval = {
    copy(start = start.map(_.plusDays(n)), end = end.map(_.plusDays(n)))
  }

  def minusYears(n: Int): LocalDateTimeInterval = {
    copy(start = start.map(_.minusYears(n)), end = end.map(_.minusYears(n)))
  }

  def minusMonths(n: Int): LocalDateTimeInterval = {
    copy(start = start.map(_.minusMonths(n)), end = end.map(_.minusMonths(n)))
  }

  def minusDays(n: Int): LocalDateTimeInterval = {
    copy(start = start.map(_.minusDays(n)), end = end.map(_.minusDays(n)))
  }

  def intersect(rhs: LocalDateTimeInterval): LocalDateTimeInterval = {
    val s = (start, rhs.start) match {
      case (None, None) => None
      case (Some(l), None) => Some(l)
      case (None, Some(r)) => Some(r)
      case (Some(l), Some(r)) => {
        if (l < r) Some(r) else Some(l)
      }
    }
    val (e, i) = (end, rhs.end) match {
      case (None, None) => (None, isEndInclusive)
      case (Some(l), None) => (Some(l), isEndInclusive)
      case (None, Some(r)) => (Some(r), rhs.isEndInclusive)
      case (Some(l), Some(r)) => {
        if (l == r) {
          if (isEndInclusive) (Some(r), rhs.isEndInclusive)
          else (Some(l), isEndInclusive)
        } else if (l < r) (Some(l), isEndInclusive)
        else (Some(r), rhs.isEndInclusive)
      }
    }
    LocalDateTimeInterval(s, e, i)
  }

  // Label
  def toDateLabel: String = {
    (start, end) match {
      case (Some(s), Some(e)) => s"${_to_date(s)}-${_to_date(e)}"
      case (Some(s), None) => s"${_to_date(s)}-"
      case (None, Some(e)) => s"-${_to_date(e)}"
      case (None, None) => ""
    }
  }

  private def _to_date(d: LocalDateTime) = LocalDateTimeUtils.toDisplayString(d)

  def localDates: Stream[LocalDate] = {
    def next(d: LocalDate): Stream[LocalDate] = {
      val cont = end match {
        case Some(e) =>
          val ed = e.toLocalDate
          if (isEndInclusive) d <= ed else d < ed
        case None => true
      }
      if (cont) d #:: next(d + 1.day) else Stream.Empty
    }
    start match {
      case Some(s) =>
        val d = s.toLocalDate
        d #:: next(d + 1.day)
      case None => throw new IllegalStateException("Missing start")
    }
  }

  def weeks(
    basedayofweek: DayOfWeek = Sunday,
    weekswitch: WeekSwitchStrategy = MiddleDaySwitch
  ): Stream[Week] = {
    sequence_logic[Week](
      Week.fromLocalDateTime(_, basedayofweek, weekswitch),
      _ + 1,
      _ <= _,
      _ < _
    )
  }

  private def _week_to_localdate(yearweek: Int): LocalDate = {
    _week_to_localdate(yearweek / 100, yearweek % 100)
  }

  private def _week_to_localdate(year: Int, week: Int): LocalDate = {
    new LocalDate().withYear(year).withWeekOfWeekyear(week)
  }

  private def _to_week(date: LocalDate): Int = {
    date.getYear * 100 + date.getWeekOfWeekyear
  }

  protected def sequence_logic[T](
    toresultf: LocalDateTime => T,
    plusonef: T => T,
    gef: (T, T) => Boolean,
    gf: (T, T) => Boolean
  ): Stream[T] = {
    def next(d: T): Stream[T] = {
      val cont = end match {
        case Some(e) =>
          val x = toresultf(e)
          if (isEndInclusive) gef(d, x) else gf(d, x)
        case None => true
      }
      if (cont) d #:: next(plusonef(d)) else Stream.Empty
    }
    start match {
      case Some(s) =>
        val x = toresultf(s)
        x #:: next(plusonef(x))
      case None => throw new IllegalStateException("Missing start")
    }
  }

  def yearMonths: Stream[YearMonth] = {
    def next(d: YearMonth): Stream[YearMonth] = {
      val cont = end match {
        case Some(e) =>
          val ed = e.toLocalDate
          val ym = new YearMonth(ed.getYear, ed.getMonthOfYear)
          if (isEndInclusive) d <= ym else d < ym
        case None => true
      }
      if (cont) d #:: next(d.plusMonths(1)) else Stream.Empty
    }
    start match {
      case Some(s) =>
        val d = s.toLocalDate
        val ym = new YearMonth(d.getYear, d.getMonthOfYear)
        ym #:: next(ym.plusMonths(1))
      case None => throw new IllegalStateException("Missing start")
    }
  }

  def years: Stream[Int] = {
    def next(d: Int): Stream[Int] = {
      val cont = end match {
        case Some(e) =>
          val ed = e.toLocalDate
          val y = new LocalDate(d, 1, 1)
          if (isEndInclusive) y <= ed else y < ed
        case None => true
      }
      if (cont) d #:: next(d + 1) else Stream.Empty
    }
    start match {
      case Some(s) =>
        val d = s.toLocalDate
        val y = d.getYear
        y #:: next(y + 1)
      case None => throw new IllegalStateException("Missing start")
    }
  }

  def dayCount: Int = {
    val (s, e) = asStartEndInclusive
    LocalDateTimeUtils.dayCount(s, e)
  }

  def dayLength: Long = {
    val (s, e) = asStartEndInclusive
    LocalDateTimeUtils.dayLength(s, e)
  }

  def asStartEnd: (LocalDateTime, LocalDateTime) = {
    (start, end) match {
      case (Some(s), Some(e)) => (s, e)
      case (Some(s), None) => throw new IllegalStateException("Missing end")
      case (None, Some(e)) => throw new IllegalStateException("Missing start")
      case (None, None) => throw new IllegalStateException("Missing both start and end")
    }
  }

  def asStartEndInclusive: (LocalDateTime, LocalDateTime) = {
    val (s, e) = asStartEnd
    if (isEndInclusive) (s, e) else (s, e - 1.millis)
  }

  def asStartEndExclusive: (LocalDateTime, LocalDateTime) = {
    val (s, e) = asStartEnd
    if (isEndInclusive) (s, e + 1.millis) else (s, e)
  }

  def getFilenamePartLocalDate: Option[String] = {
    def dt(x: LocalDateTime) = {
      LocalDateTimeUtils.toBasicDateString(x)
    }
    def delimiter = if (isEndInclusive) "-" else "--"
    (start, end) match {
      case (Some(s), Some(e)) => Some(s"${dt(s)}$delimiter${dt(e)}")
      case (Some(s), None) => Some(s"${dt(s)}$delimiter")
      case (None, Some(e)) => Some(s"$delimiter${dt(e)}")
      case (None, None) => None
    }
  }

  def toPrehistory: LocalDateTimeInterval = {
    start match {
      case Some(s) => LocalDateTimeInterval(None, start, false)
      case None => LocalDateTimeInterval.empty
    }
  }

  def marshall: String = (start, end) match {
    case (Some(s), Some(e)) => s"${_startmark}${s}~${e}${_endmark}"
    case (Some(s), None) => s"${_startmark}${s}~"
    case (None, Some(e)) => s"~${e}${_endmark}"
    case (None, None) => ""
  }

  private def _startmark = if (isStartInclusive) MARK_START_CLOSE else MARK_START_OPEN
  private def _endmark = if (isEndInclusive) MARK_END_CLOSE else MARK_END_OPEN
}

object LocalDateTimeInterval {
  implicit object LocalDateTimeIntervalMonoid extends Monoid[LocalDateTimeInterval] {
    def append(lhs: LocalDateTimeInterval, rhs: => LocalDateTimeInterval) = lhs intersect rhs
    def zero = empty
  }

  val MARK_OPEN = IntervalFactory.MARK_OPEN.head
  val MARK_START_OPEN = IntervalFactory.MARK_START_OPEN.head
  val MARK_START_CLOSE = IntervalFactory.MARK_START_CLOSE.head
  val MARK_END_OPEN = IntervalFactory.MARK_END_OPEN.head
  val MARK_END_CLOSE = IntervalFactory.MARK_END_CLOSE.head

  val Compute = """([^-]+)_([+-])(\d+)""".r
  val Year = """(\d+)""".r
  val YearMonth = """(\d+)-(\d+)""".r
  val YearMonthDay = """(\d+)-(\d+)-(\d+)""".r
  val KEY_DAY_PRESISE_TIME = "dayt"

  val empty = new LocalDateTimeInterval(None, None)

  val THREASHOLD_DAY = 1
  val THREASHOLD_HOUR = 6

  def effectiveYearMonth(year: Int, month: Int, day: Int): (Int, Int) = {
    if (day <= THREASHOLD_DAY) {
      if (month == 1)
        (year - 1, 12)
      else
        (year, month - 1)
    } else
      (year, month)
  }

  def effectiveYearMonthDay(tz: DateTimeZone)(year: Int, month: Int, day: Int, hour: Int): (Int, Int, Int) = {
    val dt = if (hour <= THREASHOLD_HOUR) {
      new DateTime(year, month, day, 0, 0, tz).
        minusDays(2)
    } else {
      new DateTime(year, month, day, 0, 0, tz).
        minusDays(1)
    }
    (dt.getYear, dt.getMonthOfYear, dt.getDayOfMonth)
  }

  def atOrAbove(year: Int, month: Int, day: Int): LocalDateTimeInterval =
    LocalDateTimeInterval(Some(new LocalDateTime(year, month, day, 0, 0)), None)

  def atOrAbove(year: Int, month: Int, day: Int, hour: Int, minute: Int, second: Int): LocalDateTimeInterval =
    LocalDateTimeInterval(Some(new LocalDateTime(year, month, day, hour, minute, second)), None)

  def openUpper(start: DateTime, end: DateTime): LocalDateTimeInterval =
    LocalDateTimeInterval(Some(start.toLocalDateTime), Some(end.toLocalDateTime), true, false)

  /*
   * Caution: Timezone depends on running environment.
   */
  def parse(p: String): ParseResult[LocalDateTimeInterval] = parse(DateTimeContext.now, CloseClose, p)

  def parseCloseOpen(p: String): ParseResult[LocalDateTimeInterval] = parse(DateTimeContext.now, CloseOpen, p)

  def parse(
    ctx: DateTimeContext,
    p: String
  ): ParseResult[LocalDateTimeInterval] = parse(ctx, CloseClose, p)

  def parse(
    ctx: DateTimeContext,
    kind: BoundsKind,
    p: String
  ): ParseResult[LocalDateTimeInterval] = ParseResult(create(ctx, kind, p))

  /*
   * Caution: Timezone depends on running environment.
   */
  def parseOption(p: String): Option[LocalDateTimeInterval] = parse(p).toOption

  def parseOption(ctx: DateTimeContext, p: String): Option[LocalDateTimeInterval] = parse(ctx, p).toOption

  def create(p: String): LocalDateTimeInterval = create(DateTimeContext.now, CloseClose, p)

  def create(
    ctx: DateTimeContext,
    kind: BoundsKind,
    p: String
  ): LocalDateTimeInterval =
    DateTimePeriod.Builder(ctx, kind).fromExpression(p).toLocalDateTimeInterval

  // case class Builder(
  //   context: DateTimeContext,
  //   kind: BoundsKind = CloseClose
  // ) {
  //   def isStartInclusive: Boolean = kind.isStartInclusive
  //   def isEndInclusive: Boolean = kind.isEndInclusive

  //   def currentYear: Int = context.currentYear
  //   def currentMonth: Int = context.currentMonth 
  //   def currentWeek: Int = context.currentWeek
  //   def currentDay: Int = context.currentDay
  //   def currentHour: Int = context.currentHour
  //   def timezoneJoda: DateTimeZone = context.timezoneJoda

  //   def create(
  //     start: Option[String],
  //     end: Option[String]
  //   ): LocalDateTimeInterval = create(start, end, isEndInclusive)

  //   def create(
  //     start: Option[String],
  //     end: Option[String],
  //     inclusive: Boolean
  //   ): LocalDateTimeInterval = {
  //     LocalDateTimeInterval(
  //       start.map(toLocalDateTime),
  //       end.map(toLocalDateTime),
  //       isStartInclusive,
  //       inclusive
  //     )
  //   }

  //   def create(
  //     start: Option[String],
  //     end: Option[String],
  //     low: Boolean,
  //     high: Boolean
  //   ): LocalDateTimeInterval = {
  //     LocalDateTimeInterval(
  //       start.map(toLocalDateTime),
  //       end.map(toLocalDateTime),
  //       low,
  //       high
  //     )
  //   }

  //   def toLocalDateTime(
  //     s: String
  //   ): LocalDateTime = {
  //     def plain(a: String) = a match {
  //       case YearMonthDay(y, m, d) =>
  //         new LocalDateTime(y.toInt, m.toInt, d.toInt, 0, 0)
  //       case YearMonth(y, m) =>
  //         new LocalDateTime(y.toInt, m.toInt, 1, 0, 0)
  //       case Year(y) =>
  //         new LocalDateTime(y.toInt, 1, 1, 0, 0)
  //       case _ => _parse(a)
  //     }
  //     def plus(a: String, n: Int) = a match {
  //       case YearMonthDay(y, m, d) =>
  //         new LocalDateTime(y.toInt, m.toInt, d.toInt, 0, 0).plusDays(n)
  //       case YearMonth(y, m) =>
  //         new LocalDateTime(y.toInt, m.toInt, 1, 0, 0).plusMonths(n)
  //       case Year(y) =>
  //         new LocalDateTime(y.toInt, 1, 1, 0, 0).plusYears(n)
  //     }
  //     def minus(a: String, n: Int) = a match {
  //       case YearMonthDay(y, m, d) =>
  //         new LocalDateTime(y.toInt, m.toInt, d.toInt, 0, 0).minusDays(n)
  //       case YearMonth(y, m) =>
  //         new LocalDateTime(y.toInt, m.toInt, 1, 0, 0).minusMonths(n)
  //       case Year(y) =>
  //         new LocalDateTime(y.toInt, 1, 1, 0, 0).minusYears(n)
  //     }
  //     try {
  //       s match {
  //         case Compute(x, "+", n) => plus(x, n.toInt)
  //         case Compute(x, "-", n) => minus(x, n.toInt)
  //         case _ => plain(s)
  //       }
  //     } catch {
  //       case NonFatal(e) => throw new IllegalArgumentException("Illegal datetime = " + s)
  //     }
  //   }

  //   def toLocalDateTimeOrLocalDateTimeInterval(s: String): Either[LocalDateTime, LocalDateTimeInterval] =
  //     if (s.contains("~"))
  //       Right(fromExpression(s))
  //     else
  //       Left(_parse(s))

  //   def toLocalDateOrLocalDateTimeInterval(s: String): Either[LocalDate, LocalDateTimeInterval] =
  //     if (s.contains("~"))
  //       Right(fromExpression(s))
  //     else
  //       Left(AnyUtils.toLocalDate(s))

  //   def fromExpression(s: String): LocalDateTimeInterval = {
  //     fromExpression(context.current, s)
  //   }

  //   def fromExpression(base: DateTime, s: String): LocalDateTimeInterval = {
  //     val dt = RichDateTime(base)
  //     val tz = dt.toRichDateTimeZone
  //     def year(y: Int) = {
  //       LocalDateTimeInterval(
  //         Some(tz.yearFirst(y).toLocalDateTime),
  //         Some(tz.yearLast(y).toLocalDateTime))
  //     }
  //     def yearmonth(y: Int, m: Int) = {
  //       LocalDateTimeInterval(
  //         Some(tz.monthFirst(y, m).toLocalDateTime),
  //         Some(tz.monthLast(y, m).toLocalDateTime))
  //     }
  //     def yearmonthday(y: Int, m: Int, d: Int) = {
  //       LocalDateTimeInterval(
  //         Some(tz.dayFirst(y, m, d).toLocalDateTime),
  //         Some(tz.dayLast(y, m, d).toLocalDateTime))
  //     }

  //     def plain_one(s: String): LocalDateTimeInterval = s match {
  //       case "year" => year(base.getYear)
  //       case "month" => yearmonth(base.getYear, base.getMonthOfYear)
  //       case "day" => yearmonthday(base.getYear, base.getMonthOfYear, base.getDayOfMonth)
  //       case KEY_DAY_PRESISE_TIME => LocalDateTimeInterval(Some(base), Some(base.plusDays(1).minusMillis(1)))
  //       case Year(y) => year(y.toInt)
  //       case YearMonth(y, m) => yearmonth(y.toInt, m.toInt)
  //       case YearMonthDay(y, m, d) => yearmonthday(y.toInt, m.toInt, d.toInt)
  //       case _ => throw new IllegalArgumentException(s"Invalid period: $s")
  //     }
  //     def plus_one(s: String, n: Int): LocalDateTimeInterval = s match {
  //       case "year" => year(base.getYear).plusYears(n)
  //       case "month" => yearmonth(base.getYear, base.getMonthOfYear).plusMonths(n)
  //       case "day" => yearmonthday(base.getYear, base.getMonthOfYear, base.getDayOfMonth).plusDays(n)
  //       case KEY_DAY_PRESISE_TIME => LocalDateTimeInterval(Some(base), Some(base.plusDays(n).minusMillis(1)))
  //       case Year(y) => year(y.toInt).plusYears(n)
  //       case YearMonth(y, m) => yearmonth(y.toInt, m.toInt).plusMonths(n)
  //       case YearMonthDay(y, m, d) => yearmonthday(y.toInt, m.toInt, d.toInt).plusDays(n)
  //       case _ => throw new IllegalArgumentException(s"Invalid period: $s, $n")
  //     }
  //     def minus_one(s: String, n: Int): LocalDateTimeInterval = s match {
  //       case "year" => year(base.getYear).minusYears(n)
  //       case "month" => yearmonth(base.getYear, base.getMonthOfYear).minusMonths(n)
  //       case "day" => yearmonthday(base.getYear, base.getMonthOfYear, base.getDayOfMonth).minusDays(n)
  //       case KEY_DAY_PRESISE_TIME => LocalDateTimeInterval(Some(base.minusDays(n)), Some(base.minusDays(n - 1).minusMillis(1)))
  //       case Year(y) => year(y.toInt).minusYears(n)
  //       case YearMonth(y, m) => yearmonth(y.toInt, m.toInt).minusMonths(n)
  //       case YearMonthDay(y, m, d) => yearmonthday(y.toInt, m.toInt, d.toInt).minusDays(n)
  //       case _ => throw new IllegalArgumentException(s"Invalid period: $s, $n")
  //     }

  //     def plain_start(s: String): DateTime = s match {
  //       case "year" => tz.yearFirst(base.getYear)
  //       case "month" => tz.monthFirst(base.getYear, base.getMonthOfYear)
  //       case "day" => tz.dayFirst(base.getYear, base.getMonthOfYear, base.getDayOfMonth)
  //       case KEY_DAY_PRESISE_TIME => base
  //       case Year(y) => tz.yearFirst(y.toInt)
  //       case YearMonth(y, m) => tz.monthFirst(y.toInt, m.toInt)
  //       case YearMonthDay(y, m, d) => tz.dayFirst(y.toInt, m.toInt, d.toInt)
  //       case _ => parseDateTime(s, tz.underlying)
  //     }

  //     def plus_start(s: String, n: Int) = s match {
  //       case "year" => tz.yearFirst(base.getYear).plusYears(n)
  //       case "month" => tz.monthFirst(base.getYear, base.getMonthOfYear).plusMonths(n)
  //       case "day" => tz.dayFirst(base.getYear, base.getMonthOfYear, base.getDayOfMonth).plusDays(n)
  //       case KEY_DAY_PRESISE_TIME => base.plusDays(n)
  //       case Year(y) => tz.yearFirst(y.toInt).plusYears(n)
  //       case YearMonth(y, m) => tz.monthFirst(y.toInt, m.toInt).plusMonths(n)
  //       case YearMonthDay(y, m, d) => tz.dayFirst(y.toInt, m.toInt, d.toInt).plusDays(n)
  //       case _ => throw new IllegalArgumentException(s"Invalid period: $s, $n")
  //     }

  //     def minus_start(s: String, n: Int) = s match {
  //       case "year" => tz.yearFirst(base.getYear).minusYears(n)
  //       case "month" => tz.monthFirst(base.getYear, base.getMonthOfYear).minusMonths(n)
  //       case "day" => tz.dayFirst(base.getYear, base.getMonthOfYear, base.getDayOfMonth).minusDays(n)
  //       case KEY_DAY_PRESISE_TIME => base.minusDays(n)
  //       case Year(y) => tz.yearFirst(y.toInt).minusYears(n)
  //       case YearMonth(y, m) => tz.monthFirst(y.toInt, m.toInt).minusMonths(n)
  //       case YearMonthDay(y, m, d) => tz.dayFirst(y.toInt, m.toInt, d.toInt).minusDays(n)
  //       case _ => throw new IllegalArgumentException(s"Invalid period: $s, $n")
  //     }

  //     def plain_end(s: String) = s match {
  //       case "year" => tz.yearLast(base.getYear)
  //       case "month" => tz.monthLast(base.getYear, base.getMonthOfYear)
  //       case "day" => tz.dayLast(base.getYear, base.getMonthOfYear, base.getDayOfMonth)
  //       case KEY_DAY_PRESISE_TIME => base.plusDays(1).minusMillis(1)
  //       case Year(y) => tz.yearLast(y.toInt)
  //       case YearMonth(y, m) => tz.monthLast(y.toInt, m.toInt)
  //       case YearMonthDay(y, m, d) => tz.dayLast(y.toInt, m.toInt, d.toInt)
  //       case _ => parseDateTime(s, tz.underlying)
  //     }

  //     def plus_end(s: String, n: Int) = s match {
  //       case "year" => tz.yearLast(base.getYear).plusYears(n)
  //       case "month" => tz.monthLast(base.getYear, base.getMonthOfYear).plusMonths(n)
  //       case "day" => tz.dayLast(base.getYear, base.getMonthOfYear, base.getDayOfMonth).plusDays(n)
  //       case KEY_DAY_PRESISE_TIME => base.plusDays(n + 1).minusMillis(1)
  //       case Year(y) => tz.yearLast(y.toInt).plusYears(n)
  //       case YearMonth(y, m) => tz.monthLast(y.toInt, m.toInt).plusMonths(n)
  //       case YearMonthDay(y, m, d) => tz.dayLast(y.toInt, m.toInt, d.toInt).plusDays(n)
  //       case _ => throw new IllegalArgumentException(s"Invalid period: $s, $n")
  //     }

  //     def minus_end(s: String, n: Int) = s match {
  //       case "year" => tz.yearLast(base.getYear).minusYears(n)
  //       case "month" => tz.monthLast(base.getYear, base.getMonthOfYear).minusMonths(n)
  //       case "day" => tz.dayLast(base.getYear, base.getMonthOfYear, base.getDayOfMonth).minusDays(n)
  //       case KEY_DAY_PRESISE_TIME => base.minusDays(n - 1).minusMillis(1)
  //       case Year(y) => tz.yearLast(y.toInt).minusYears(n)
  //       case YearMonth(y, m) => tz.monthLast(y.toInt, m.toInt).minusMonths(n)
  //       case YearMonthDay(y, m, d) => tz.dayLast(y.toInt, m.toInt, d.toInt).minusDays(n)
  //       case _ => throw new IllegalArgumentException(s"Invalid period: $s, $n")
  //     }

  //     def body(inclusivep: Boolean, sb: String, si: Boolean, ei: Boolean): LocalDateTimeInterval = {
  //       if (!sb.contains("~")) {
  //         sb match {
  //           case Compute(x, "+", n) => plus_one(x, n.toInt)
  //           case Compute(x, "-", n) => minus_one(x, n.toInt)
  //           case _ => plain_one(sb)
  //         }
  //       } else {
  //         val r = """([^~]+)?~(.+)?""".r
  //         val r(start, end) = sb
  //         val s1: Option[DateTime] = Option(start) map {
  //           case Compute(x, "+", n) => plus_start(x, n.toInt)
  //           case Compute(x, "-", n) => minus_start(x, n.toInt)
  //           case _ => plain_start(start)
  //         }
  //         val e1: Option[DateTime] = if (inclusivep) {
  //           Option(end) map {
  //             case Compute(x, "+", n) => plus_end(x, n.toInt)
  //             case Compute(x, "-", n) => minus_end(x, n.toInt)
  //             case _ => plain_end(end)
  //           }
  //         } else {
  //           Option(end) map {
  //             case Compute(x, "+", n) => plus_start(x, n.toInt)
  //             case Compute(x, "-", n) => minus_start(x, n.toInt)
  //             case _ => plain_start(end)
  //           }
  //         }
  //         LocalDateTimeInterval(s1, e1, si, ei)
  //       }
  //     }

  //     if (s.isEmpty) {
  //       RAISE.invalidArgumentFault("Empty datetime")
  //     } else {
  //       val (a, isstartinclusive) = s.head match {
  //         case MARK_START_OPEN => (s.tail, false)
  //         case MARK_START_CLOSE => (s.tail, true)
  //         case _ => (s, isStartInclusive)
  //       }
  //       val (b, isendinclusive) = a.last match {
  //         case MARK_START_OPEN => (a.init, false)
  //         case MARK_START_CLOSE => (a.init, true)
  //         case MARK_OPEN => (a.init, false)
  //         case _ => (a, isEndInclusive)
  //       }
  //       body(false, b, isstartinclusive, isendinclusive)
  //     }
  //   }

  //   def yearly: LocalDateTimeInterval = {
  //     yearly(currentYear)
  //   }

  //   def effectiveYearly: LocalDateTimeInterval = {
  //     yearly
  //   }

  //   def yearly(
  //     year: Int
  //   ): LocalDateTimeInterval = {
  //     LocalDateTimeInterval(
  //       Some(datetimeYearFirst(year)),
  //       Some(datetimeYearLast(year)))
  //   }

  //   def monthly: LocalDateTimeInterval = {
  //     monthly(currentYear, currentMonth)
  //   }

  //   def effectiveMonthly: LocalDateTimeInterval = {
  //     val (year, month) = effectiveYearMonth(currentYear, currentMonth, currentDay)
  //     val start = new DateTime(year, month, 1, 0, 0)
  //     val end = new DateTime(year, month, 1, 0, 0).plusMonths(1).minusDays(1)
  //     LocalDateTimeInterval(Some(start), Some(end))
  //   }

  //   def monthly(
  //     year: Int, month: Int
  //   ): LocalDateTimeInterval = {
  //     LocalDateTimeInterval(
  //       Some(datetimeMonthFirst(year, month)),
  //       Some(datetimeMonthLast(year, month)))
  //   }

  //   def weekly: LocalDateTimeInterval = {
  //     weekly(currentYear, currentWeek)
  //   }

  //   def effectiveWeekly: LocalDateTimeInterval = {
  //     weekly // XXX
  //   }

  //   def weekly(
  //     year: Int, week: Int
  //   ): LocalDateTimeInterval = {
  //     LocalDateTimeInterval(
  //       Some(datetimeWeekFirst(year, week)),
  //       Some(datetimeWeekLast(year, week)))
  //   }

  //   def daily: LocalDateTimeInterval = {
  //     daily(currentYear, currentMonth, currentDay)
  //   }

  //   def effectiveDaily: LocalDateTimeInterval = {
  //     val (year, month, day) = effectiveYearMonthDay(timezoneJoda)(currentYear, currentMonth, currentDay, currentHour)
  //     val start = new DateTime(year, month, day, 0, 0)
  //     val end = new DateTime(year, month, day, 0, 0).plusDays(1).minusMillis(1)
  //     LocalDateTimeInterval(Some(start), Some(end))
  //   }

  //   def daily(
  //     year: Int, month: Int, day: Int
  //   ): LocalDateTimeInterval = {
  //     LocalDateTimeInterval(
  //       Some(datetimeDayFirst(year, month, day)),
  //       Some(datetimeDayLast(year, month, day)))
  //   }

  //   def datetimeYearFirst(year: Int): DateTime = {
  //     new DateTime(year, 1, 1, 0, 0)
  //   }

  //   def datetimeYearLast(year: Int): DateTime = {
  //     val dt = new DateTime(year, 1, 1, 0, 0)
  //     dt.plusYears(1).minusMillis(1)
  //   }

  //   def datetimeMonthFirst(year: Int, month: Int): DateTime = {
  //     new DateTime(year, month, 1, 0, 0)
  //   }

  //   def datetimeMonthLast(year: Int, month: Int): DateTime = {
  //     val dt = new DateTime(year, month, 1, 0, 0)
  //     dt.plusMonths(1).minusMillis(1)
  //   }

  //   def datetimeWeekFirst(year: Int, week: Int): DateTime = {
  //     ???
  //   }

  //   def datetimeWeekLast(year: Int, week: Int): DateTime = {
  //     // val dt = new DateTime(year, month, 1, 0, 0)
  //     // dt.plusMonths(1).minusMillis(1)
  //     ???
  //   }

  //   def datetimeDayFirst(year: Int, month: Int, day: Int): DateTime = {
  //     new DateTime(year, month, day, 0, 0)
  //   }

  //   def datetimeDayLast(year: Int, month: Int, day: Int): DateTime = {
  //     val dt = new DateTime(year, month, day, 0, 0)
  //     dt.plusDays(1).minusMillis(1)
  //   }

  //   private def _parse(s: String): LocalDateTime = LocalDateTimeUtils.parseIso(s)
  // }

  // private def gmt = DateTimeUtils.gmt
  // private def jodajst = DateTimeUtils.jodajst

  // private def sql_long2datetime(t: Long): String = {
  //   val format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
  //   format.setTimeZone(gmt)
  //   val d = new java.sql.Date(t)
  //   sqlLiteral(format.format(d))
  // }

  // protected def sql_long2date(t: Long): String = {
  //   val format = new SimpleDateFormat("yyyy-MM-dd")
  //   format.setTimeZone(gmt)
  //   val d = new java.sql.Date(t)
  //   val r = sqlLiteral(format.format(d))
  //   r
  // }

  // private def sqlLiteral(d: Any): String = {
  //   "'" + sqlEscape(d.toString) + "'"
  // }

  // private def sqlEscape(s: String): String = {
  //   if (s.indexOf("'") == -1) s.replace("\\", "\\\\")
  //   else s.replace("'", "''").replace("\\", "\\\\")
  // }

  // private def sqlLiteralList(xs: Seq[String]): String =
  //   xs.map(sqlLiteral).mkString(", ")

  // private def toSqlString(ts: Long): String = {
  //   val df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
  //   df.setTimeZone(gmt)
  //   df.format(ts)
  // }

  // private def toSqlLiteral(dt: DateTime): String = toSqlLiteral(dt.getMillis)

  // private def toSqlLiteral(ts: Long): String = "'" + toSqlString(ts) + "'"
}
