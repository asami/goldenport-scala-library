package org.goldenport.values

import scalaz._, Scalaz._
import scala.util.Try
import scala.util.control.NonFatal
import java.util.Date
import java.text.SimpleDateFormat
import java.sql.Timestamp
import com.github.nscala_time.time.Imports._
import org.joda.time.DateTimeConstants._
import spire.math.Interval
import spire.math.interval._
import org.goldenport.util.{DateTimeUtils, DateUtils, AnyUtils}
import org.goldenport.util.SpireUtils.Implicits._
import org.goldenport.values.Week._

/*
 * @since   Oct.  2, 2014
 *  version Nov. 19, 2014
 *  version Feb. 21, 2015
 *  version May. 15, 2015
 *  version Jun. 18, 2015
 *  version Oct. 22, 2015
 *  version Nov. 19, 2015
 *  version Feb.  5, 2018
 *  version Jun. 20, 2018
 *  version Jan. 10, 2019
 *  version Aug. 14, 2019
 * @version Sep. 18, 2019
 * @author  ASAMI, Tomoharu
 */
case class DateTimePeriod( // TODO DateTimeInterval (java8 time)
  start: Option[DateTime],
  end: Option[DateTime],
  isStartInclusive: Boolean = true,
  isEndInclusive: Boolean = true
) {
  import DateTimePeriod._

  def toInterval: Interval[DateTime] = {
    val lower = _to_bound(start, isStartInclusive)
    val higher = _to_bound(end, isEndInclusive)
    Interval.fromBounds(lower, higher)
  }

  private def _to_bound(p: Option[DateTime], inclusive: Boolean): Bound[DateTime] =
    p.map(x => if (inclusive) Closed(x) else Open(x)).getOrElse(Unbound())

  def isValid(p: DateTime): Boolean = isValid(p.getMillis)

  def isValid(timestamp: Timestamp): Boolean = isValid(timestamp.getTime)

  // Assumes GMT
  def isValid(date: Date): Boolean = {
    val ts = DateUtils.toTimestampJst(date)
    isValid(ts)
  }

  def isValid(ts: Long): Boolean = (start, end) match {
    case (None, None) => true
    case (Some(s), None) => s.getMillis <= ts
    case (None, Some(e)) => ts <= e.getMillis
    case (Some(s), Some(e)) => s.getMillis <= ts && ts <= e.getMillis
  }

  def startTimestamp: Option[java.sql.Timestamp] = {
    start.map(s => new java.sql.Timestamp(s.getMillis))
  }

  def startLocalDate: Option[LocalDate] = start.map(_.toLocalDate)

  def endTimestamp: Option[java.sql.Timestamp] = {
    end.map(s => new java.sql.Timestamp(s.getMillis))
  }

  def endLocalDate: Option[LocalDate] = end.map(_.toLocalDate)

  def withEndNowIfRequired: DateTimePeriod = {
    if (end.nonEmpty)
      this
    else
      copy(end = Some(DateTime.now(DateTimeUtils.jodajst))) // TODO TZ
  }

  def plusYears(n: Int): DateTimePeriod = {
    copy(start = start.map(_.plusYears(n)), end = end.map(_.plusYears(n)))
  }

  def plusMonths(n: Int): DateTimePeriod = {
    copy(start = start.map(_.plusMonths(n)), end = end.map(_.plusMonths(n)))
  }

  def plusDays(n: Int): DateTimePeriod = {
    copy(start = start.map(_.plusDays(n)), end = end.map(_.plusDays(n)))
  }

  def minusYears(n: Int): DateTimePeriod = {
    copy(start = start.map(_.minusYears(n)), end = end.map(_.minusYears(n)))
  }

  def minusMonths(n: Int): DateTimePeriod = {
    copy(start = start.map(_.minusMonths(n)), end = end.map(_.minusMonths(n)))
  }

  def minusDays(n: Int): DateTimePeriod = {
    copy(start = start.map(_.minusDays(n)), end = end.map(_.minusDays(n)))
  }

  def intersect(rhs: DateTimePeriod): DateTimePeriod = {
    val s = (start, rhs.start) match {
      case (None, None) => None
      case (Some(l), None) => Some(l)
      case (None, Some(r)) => Some(r)
      case (Some(l), Some(r)) => {
        if (l.getMillis < r.getMillis) Some(r) else Some(l)
      }
    }
    val (e, i) = (end, rhs.end) match {
      case (None, None) => (None, isEndInclusive)
      case (Some(l), None) => (Some(l), isEndInclusive)
      case (None, Some(r)) => (Some(r), rhs.isEndInclusive)
      case (Some(l), Some(r)) => {
        if (l.getMillis == r.getMillis) {
          if (isEndInclusive) (Some(r), rhs.isEndInclusive)
          else (Some(l), isEndInclusive)
        } else if (l.getMillis < r.getMillis) (Some(l), isEndInclusive)
        else (Some(r), rhs.isEndInclusive)
      }
    }
    DateTimePeriod(s, e, i)
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

  private def _to_date(d: DateTime) = DateUtils.toDisplayString(d)

  // SQL
  def toSqlDate(column: String): String = {
    s"""$column $toSqlBetweenDate"""
  }

  def toSqlBetweenDate: String = {
    def f(td: DateTime) = sql_long2date(td.getMillis) // GMT
    (start, end) match {
      case (Some(s), Some(e)) => 
        val s1 = f(s)
        val e1 = f(e)
        s" BETWEEN $s1 and $e1"
      case (Some(s), None) =>
        val s1 = f(s)
        s" >= $s1"
      case (None, Some(e)) => 
        val e1 = f(e)
        if (isEndInclusive)
          s" <= $e1"
        else
          s" < $e1"
      case (None, None) =>
        throw new IllegalArgumentException("Missing both start and end")
    }
  }

  def toSqlDateTime(column: String): String = {
    s"""$column $toSqlBetweenDateTime"""
  }

  def toSqlBetweenDateTime: String = {
    (start, end) match {
      case (Some(s), Some(e)) => 
        val s1 = toSqlLiteral(s)
        val e1 = toSqlLiteral(
          if (isEndInclusive)
            e
          else
            e - 1.millis
        )
        s" BETWEEN $s1 and $e1"
      case (Some(s), None) =>
        val s1 = toSqlLiteral(s)
        s" >= $s1"
      case (None, Some(e)) => 
        val e1 = toSqlLiteral(e)
        if (isEndInclusive)
          s" <= $e1"
        else
          s" < $e1"
      case (None, None) =>
        throw new IllegalArgumentException("Missing both start and end")
    }
  }

  def betweenDateTime: (Option[String], Option[String]) = {
    def f(x: DateTime) = toSqlLiteral(x)
    val s = start.map(f)
    val e = end.map(x => f(if (isEndInclusive) x else x - 1.millis))
    (s, e)
  }

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
      Week.fromDateTime(_, basedayofweek, weekswitch),
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
    toresultf: DateTime => T,
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

  def dayCount: Long = {
    val (s, e) = asStartEndInclusive
    val a = e.withTimeAtStartOfDay.getMillis - s.withTimeAtStartOfDay.getMillis + 1
    _count_day(a)
  }

  def dayLength: Long = {
    val (s, e) = asStartEndInclusive
    val a = e.getMillis - s.getMillis + 1
    _count_day(a)
  }

  private def _count_day(x: Long): Long = {
    val round = if (x % MILLIS_PER_MINUTE == 0) 0 else 1
    (x / MILLIS_PER_DAY) + round
  }

  def asStartEnd: (DateTime, DateTime) = {
    (start, end) match {
      case (Some(s), Some(e)) => (s, e)
      case (Some(s), None) => throw new IllegalStateException("Missing end")
      case (None, Some(e)) => throw new IllegalStateException("Missing start")
      case (None, None) => throw new IllegalStateException("Missing both start and end")
    }
  }

  def asStartEndInclusive: (DateTime, DateTime) = {
    val (s, e) = asStartEnd
    if (isEndInclusive) (s, e) else (s, e - 1.millis)
  }

  def asStartEndExclusive: (DateTime, DateTime) = {
    val (s, e) = asStartEnd
    if (isEndInclusive) (s, e + 1.millis) else (s, e)
  }

  def getFilenamePartLocalDate: Option[String] = {
    def dt(x: DateTime) = {
      DateTimeUtils.toBasicDateStringJst(x)
    }
    def delimiter = if (isEndInclusive) "-" else "--"
    (start, end) match {
      case (Some(s), Some(e)) => Some(s"${dt(s)}$delimiter${dt(e)}")
      case (Some(s), None) => Some(s"${dt(s)}$delimiter")
      case (None, Some(e)) => Some(s"$delimiter${dt(e)}")
      case (None, None) => None
    }
  }

  def toNaturalJst: String = {
    DateTimeUtils.toNaturalStringJst(start, end, isEndInclusive)
  }

  def toPrehistory: DateTimePeriod = {
    start match {
      case Some(s) => DateTimePeriod(None, start, false)
      case None => DateTimePeriod.empty
    }
  }
}

object DateTimePeriod {
  implicit object DateTimePeriodMonoid extends Monoid[DateTimePeriod] {
    def append(lhs: DateTimePeriod, rhs: => DateTimePeriod) = lhs intersect rhs
    def zero = empty
  }

  val Compute = """([^-]+)_([+-])(\d+)""".r
  val Year = """(\d+)""".r
  val YearMonth = """(\d+)-(\d+)""".r
  val YearMonthDay = """(\d+)-(\d+)-(\d+)""".r
  val KEY_DAY_PRESISE_TIME = "dayt"

  val empty = new DateTimePeriod(None, None)

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

  def atOrAboveJst(year: Int, month: Int, day: Int): DateTimePeriod =
    DateTimePeriod(Some(new DateTime(year, month, day, 0, 0, jodajst)), None)

  /*
   * Caution: Timezone depends running environment.
   */
  def parse(p: String): DateTimePeriod = parse(DateTime.now, p)

  def parse(now: DateTime, p: String): DateTimePeriod = parse(now, now.getZone, p)

  def parse(tz: DateTimeZone, p: String): DateTimePeriod = parse(DateTime.now, tz, p)

  def parse(now: DateTime, tz: DateTimeZone, p: String): DateTimePeriod =
    Builder(now, tz).fromExpression(p)

  def parseJst(p: String): DateTimePeriod = parse(jodajst, p)

  /*
   * Caution: Timezone depends running environment.
   */
  def parseOption(p: String): Option[DateTimePeriod] = Try(parse(p)).toOption

  case class Builder(
    datetime: DateTime,
    timezoneJoda: DateTimeZone
  ) {
    def currentYear: Int = datetime.year.get
    def currentMonth: Int = datetime.monthOfYear.get
    def currentWeek: Int = datetime.weekOfWeekyear.get
    def currentDay: Int = datetime.dayOfMonth.get
    def currentHour: Int = datetime.hourOfDay.get

    def create(
      start: Option[String],
      end: Option[String]
    ): DateTimePeriod = create(start, end, true)

    def create(
      start: Option[String],
      end: Option[String],
      inclusive: Boolean
    ): DateTimePeriod = {
      DateTimePeriod(
        start.map(toDateTime),
        end.map(toDateTime),
        true,
        inclusive
      )
    }

    def create(
      start: Option[String],
      end: Option[String],
      low: Boolean,
      high: Boolean
    ): DateTimePeriod = {
      DateTimePeriod(
        start.map(toDateTime),
        end.map(toDateTime),
        low,
        high
      )
    }

    def toDateTime(
      s: String
    ): DateTime = {
      def plain(a: String) = a match {
        case YearMonthDay(y, m, d) =>
          new DateTime(y.toInt, m.toInt, d.toInt, 0, 0, timezoneJoda)
        case YearMonth(y, m) =>
          new DateTime(y.toInt, m.toInt, 1, 0, 0, timezoneJoda)
        case Year(y) =>
          new DateTime(y.toInt, 1, 1, 0, 0, timezoneJoda)
        case _ => datetimeParse(a)
      }
      def plus(a: String, n: Int) = a match {
        case YearMonthDay(y, m, d) =>
          new DateTime(y.toInt, m.toInt, d.toInt, 0, 0, timezoneJoda).plusDays(n)
        case YearMonth(y, m) =>
          new DateTime(y.toInt, m.toInt, 1, 0, 0, timezoneJoda).plusMonths(n)
        case Year(y) =>
          new DateTime(y.toInt, 1, 1, 0, 0, timezoneJoda).plusYears(n)
      }
      def minus(a: String, n: Int) = a match {
        case YearMonthDay(y, m, d) =>
          new DateTime(y.toInt, m.toInt, d.toInt, 0, 0, timezoneJoda).minusDays(n)
        case YearMonth(y, m) =>
          new DateTime(y.toInt, m.toInt, 1, 0, 0, timezoneJoda).minusMonths(n)
        case Year(y) =>
          new DateTime(y.toInt, 1, 1, 0, 0, timezoneJoda).minusYears(n)
      }
      try {
        s match {
          case Compute(x, "+", n) => plus(x, n.toInt)
          case Compute(x, "-", n) => minus(x, n.toInt)
          case _ => plain(s)
        }
      } catch {
        case NonFatal(e) => throw new IllegalArgumentException("Illegal datetime = " + s)
      }
    }

    def toDateTimeOrDateTimePeriod(s: String): Either[DateTime, DateTimePeriod] =
      if (s.contains("~"))
        Right(fromExpression(s))
      else
        Left(toDateTime(s))

    def toDateOrDateTimePeriod(s: String): Either[Date, DateTimePeriod] =
      if (s.contains("~"))
        Right(fromExpression(s))
      else
        Left(AnyUtils.toDate(s))

    def fromExpression(s: String): DateTimePeriod = {
      fromExpression(datetime, s)
    }

    def fromExpression(base: DateTime, s: String): DateTimePeriod = {
      val dt = RichDateTime(base)
      val tz = dt.toRichDateTimeZone
      def year(y: Int) = {
        DateTimePeriod(
          Some(tz.yearFirst(y)),
          Some(tz.yearLast(y)))
      }
      def yearmonth(y: Int, m: Int) = {
        DateTimePeriod(
          Some(tz.monthFirst(y, m)),
          Some(tz.monthLast(y, m)))
      }
      def yearmonthday(y: Int, m: Int, d: Int) = {
        DateTimePeriod(
          Some(tz.dayFirst(y, m, d)),
          Some(tz.dayLast(y, m, d)))
      }

      def plain_one(s: String): DateTimePeriod = s match {
        case "year" => year(base.getYear)
        case "month" => yearmonth(base.getYear, base.getMonthOfYear)
        case "day" => yearmonthday(base.getYear, base.getMonthOfYear, base.getDayOfMonth)
        case KEY_DAY_PRESISE_TIME => DateTimePeriod(Some(base), Some(base.plusDays(1).minusMillis(1)))
        case Year(y) => year(y.toInt)
        case YearMonth(y, m) => yearmonth(y.toInt, m.toInt)
        case YearMonthDay(y, m, d) => yearmonthday(y.toInt, m.toInt, d.toInt)
        case _ => throw new IllegalArgumentException(s"Invalid period: $s")
      }
      def plus_one(s: String, n: Int): DateTimePeriod = s match {
        case "year" => year(base.getYear).plusYears(n)
        case "month" => yearmonth(base.getYear, base.getMonthOfYear).plusMonths(n)
        case "day" => yearmonthday(base.getYear, base.getMonthOfYear, base.getDayOfMonth).plusDays(n)
        case KEY_DAY_PRESISE_TIME => DateTimePeriod(Some(base), Some(base.plusDays(n).minusMillis(1)))
        case Year(y) => year(y.toInt).plusYears(n)
        case YearMonth(y, m) => yearmonth(y.toInt, m.toInt).plusMonths(n)
        case YearMonthDay(y, m, d) => yearmonthday(y.toInt, m.toInt, d.toInt).plusDays(n)
        case _ => throw new IllegalArgumentException(s"Invalid period: $s, $n")
      }
      def minus_one(s: String, n: Int): DateTimePeriod = s match {
        case "year" => year(base.getYear).minusYears(n)
        case "month" => yearmonth(base.getYear, base.getMonthOfYear).minusMonths(n)
        case "day" => yearmonthday(base.getYear, base.getMonthOfYear, base.getDayOfMonth).minusDays(n)
        case KEY_DAY_PRESISE_TIME => DateTimePeriod(Some(base.minusDays(n)), Some(base.minusDays(n - 1).minusMillis(1)))
        case Year(y) => year(y.toInt).minusYears(n)
        case YearMonth(y, m) => yearmonth(y.toInt, m.toInt).minusMonths(n)
        case YearMonthDay(y, m, d) => yearmonthday(y.toInt, m.toInt, d.toInt).minusDays(n)
        case _ => throw new IllegalArgumentException(s"Invalid period: $s, $n")
      }

      def plain_start(s: String): DateTime = s match {
        case "year" => tz.yearFirst(base.getYear)
        case "month" => tz.monthFirst(base.getYear, base.getMonthOfYear)
        case "day" => tz.dayFirst(base.getYear, base.getMonthOfYear, base.getDayOfMonth)
        case KEY_DAY_PRESISE_TIME => base
        case Year(y) => tz.yearFirst(y.toInt)
        case YearMonth(y, m) => tz.monthFirst(y.toInt, m.toInt)
        case YearMonthDay(y, m, d) => tz.dayFirst(y.toInt, m.toInt, d.toInt)
        case _ => parseDateTime(s, tz.underlying)
      }

      def plus_start(s: String, n: Int) = s match {
        case "year" => tz.yearFirst(base.getYear).plusYears(n)
        case "month" => tz.monthFirst(base.getYear, base.getMonthOfYear).plusMonths(n)
        case "day" => tz.dayFirst(base.getYear, base.getMonthOfYear, base.getDayOfMonth).plusDays(n)
        case KEY_DAY_PRESISE_TIME => base.plusDays(n)
        case Year(y) => tz.yearFirst(y.toInt).plusYears(n)
        case YearMonth(y, m) => tz.monthFirst(y.toInt, m.toInt).plusMonths(n)
        case YearMonthDay(y, m, d) => tz.dayFirst(y.toInt, m.toInt, d.toInt).plusDays(n)
        case _ => throw new IllegalArgumentException(s"Invalid period: $s, $n")
      }

      def minus_start(s: String, n: Int) = s match {
        case "year" => tz.yearFirst(base.getYear).minusYears(n)
        case "month" => tz.monthFirst(base.getYear, base.getMonthOfYear).minusMonths(n)
        case "day" => tz.dayFirst(base.getYear, base.getMonthOfYear, base.getDayOfMonth).minusDays(n)
        case KEY_DAY_PRESISE_TIME => base.minusDays(n)
        case Year(y) => tz.yearFirst(y.toInt).minusYears(n)
        case YearMonth(y, m) => tz.monthFirst(y.toInt, m.toInt).minusMonths(n)
        case YearMonthDay(y, m, d) => tz.dayFirst(y.toInt, m.toInt, d.toInt).minusDays(n)
        case _ => throw new IllegalArgumentException(s"Invalid period: $s, $n")
      }

      def plain_end(s: String) = s match {
        case "year" => tz.yearLast(base.getYear)
        case "month" => tz.monthLast(base.getYear, base.getMonthOfYear)
        case "day" => tz.dayLast(base.getYear, base.getMonthOfYear, base.getDayOfMonth)
        case KEY_DAY_PRESISE_TIME => base.plusDays(1).minusMillis(1)
        case Year(y) => tz.yearLast(y.toInt)
        case YearMonth(y, m) => tz.monthLast(y.toInt, m.toInt)
        case YearMonthDay(y, m, d) => tz.dayLast(y.toInt, m.toInt, d.toInt)
        case _ => parseDateTime(s, tz.underlying)
      }

      def plus_end(s: String, n: Int) = s match {
        case "year" => tz.yearLast(base.getYear).plusYears(n)
        case "month" => tz.monthLast(base.getYear, base.getMonthOfYear).plusMonths(n)
        case "day" => tz.dayLast(base.getYear, base.getMonthOfYear, base.getDayOfMonth).plusDays(n)
        case KEY_DAY_PRESISE_TIME => base.plusDays(n + 1).minusMillis(1)
        case Year(y) => tz.yearLast(y.toInt).plusYears(n)
        case YearMonth(y, m) => tz.monthLast(y.toInt, m.toInt).plusMonths(n)
        case YearMonthDay(y, m, d) => tz.dayLast(y.toInt, m.toInt, d.toInt).plusDays(n)
        case _ => throw new IllegalArgumentException(s"Invalid period: $s, $n")
      }

      def minus_end(s: String, n: Int) = s match {
        case "year" => tz.yearLast(base.getYear).minusYears(n)
        case "month" => tz.monthLast(base.getYear, base.getMonthOfYear).minusMonths(n)
        case "day" => tz.dayLast(base.getYear, base.getMonthOfYear, base.getDayOfMonth).minusDays(n)
        case KEY_DAY_PRESISE_TIME => base.minusDays(n - 1).minusMillis(1)
        case Year(y) => tz.yearLast(y.toInt).minusYears(n)
        case YearMonth(y, m) => tz.monthLast(y.toInt, m.toInt).minusMonths(n)
        case YearMonthDay(y, m, d) => tz.dayLast(y.toInt, m.toInt, d.toInt).minusDays(n)
        case _ => throw new IllegalArgumentException(s"Invalid period: $s, $n")
      }

      def body(inclusivep: Boolean, sb: String): DateTimePeriod = {
        if (!sb.contains("~")) {
          sb match {
            case Compute(x, "+", n) => plus_one(x, n.toInt)
            case Compute(x, "-", n) => minus_one(x, n.toInt)
            case _ => plain_one(sb)
          }
        } else {
          val r = """([^~]+)?~(.+)?""".r
          val r(start, end) = sb
          val s1: Option[DateTime] = Option(start) map {
            case Compute(x, "+", n) => plus_start(x, n.toInt)
            case Compute(x, "-", n) => minus_start(x, n.toInt)
            case _ => plain_start(start)
          }
          val e1: Option[DateTime] = if (inclusivep) {
            Option(end) map {
              case Compute(x, "+", n) => plus_end(x, n.toInt)
              case Compute(x, "-", n) => minus_end(x, n.toInt)
              case _ => plain_end(end)
            }
          } else {
            Option(end) map {
              case Compute(x, "+", n) => plus_start(x, n.toInt)
              case Compute(x, "-", n) => minus_start(x, n.toInt)
              case _ => plain_start(end)
            }
          }
          DateTimePeriod(s1, e1)
        }
      }

      if (s.endsWith("!")) {
        val a = body(false, s.dropRight(1))
        a.copy(isEndInclusive = false)
      } else {
        body(true, s)
      }
    }

    def yearly: DateTimePeriod = {
      yearly(currentYear)
    }

    def effectiveYearly: DateTimePeriod = {
      yearly
    }

    def yearly(
      year: Int
    ): DateTimePeriod = {
      DateTimePeriod(
        Some(datetimeYearFirst(year)),
        Some(datetimeYearLast(year)))
    }

    def monthly: DateTimePeriod = {
      monthly(currentYear, currentMonth)
    }

    def effectiveMonthly: DateTimePeriod = {
      val (year, month) = effectiveYearMonth(currentYear, currentMonth, currentDay)
      val start = new DateTime(year, month, 1, 0, 0, timezoneJoda)
      val end = new DateTime(year, month, 1, 0, 0, timezoneJoda).plusMonths(1).minusDays(1)
      DateTimePeriod(Some(start), Some(end))
    }

    def monthly(
      year: Int, month: Int
    ): DateTimePeriod = {
      DateTimePeriod(
        Some(datetimeMonthFirst(year, month)),
        Some(datetimeMonthLast(year, month)))
    }

    def weekly: DateTimePeriod = {
      weekly(currentYear, currentWeek)
    }

    def effectiveWeekly: DateTimePeriod = {
      weekly // XXX
    }

    def weekly(
      year: Int, week: Int
    ): DateTimePeriod = {
      DateTimePeriod(
        Some(datetimeWeekFirst(year, week)),
        Some(datetimeWeekLast(year, week)))
    }

    def daily: DateTimePeriod = {
      daily(currentYear, currentMonth, currentDay)
    }

    def effectiveDaily: DateTimePeriod = {
      val (year, month, day) = effectiveYearMonthDay(timezoneJoda)(currentYear, currentMonth, currentDay, currentHour)
      val start = new DateTime(year, month, day, 0, 0, timezoneJoda)
      val end = new DateTime(year, month, day, 0, 0, timezoneJoda).plusDays(1).minusMillis(1)
      DateTimePeriod(Some(start), Some(end))
    }

    def daily(
      year: Int, month: Int, day: Int
    ): DateTimePeriod = {
      DateTimePeriod(
        Some(datetimeDayFirst(year, month, day)),
        Some(datetimeDayLast(year, month, day)))
    }

    def datetimeYearFirst(year: Int): DateTime = {
      new DateTime(year, 1, 1, 0, 0, timezoneJoda)
    }

    def datetimeYearLast(year: Int): DateTime = {
      val dt = new DateTime(year, 1, 1, 0, 0, timezoneJoda)
      dt.plusYears(1).minusMillis(1)
    }

    def datetimeMonthFirst(year: Int, month: Int): DateTime = {
      new DateTime(year, month, 1, 0, 0, timezoneJoda)
    }

    def datetimeMonthLast(year: Int, month: Int): DateTime = {
      val dt = new DateTime(year, month, 1, 0, 0, timezoneJoda)
      dt.plusMonths(1).minusMillis(1)
    }

    def datetimeWeekFirst(year: Int, week: Int): DateTime = {
      ???
    }

    def datetimeWeekLast(year: Int, week: Int): DateTime = {
      // val dt = new DateTime(year, month, 1, 0, 0, timezoneJoda)
      // dt.plusMonths(1).minusMillis(1)
      ???
    }

    def datetimeDayFirst(year: Int, month: Int, day: Int): DateTime = {
      new DateTime(year, month, day, 0, 0, timezoneJoda)
    }

    def datetimeDayLast(year: Int, month: Int, day: Int): DateTime = {
      val dt = new DateTime(year, month, day, 0, 0, timezoneJoda)
      dt.plusDays(1).minusMillis(1)
    }

    def datetimeParse(s: String): DateTime = parseDateTime(s, timezoneJoda)

    def parseDateTime(s: String, tz: DateTimeZone): DateTime =
      DateTimeUtils.parseIsoDateTime(s, tz)
  }

  private def gmt = DateTimeUtils.gmt
  private def jodajst = DateTimeUtils.jodajst

  private def sql_long2datetime(t: Long): String = {
    val format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    format.setTimeZone(gmt)
    val d = new java.sql.Date(t)
    sqlLiteral(format.format(d))
  }

  protected def sql_long2date(t: Long): String = {
    val format = new SimpleDateFormat("yyyy-MM-dd")
    format.setTimeZone(gmt)
    val d = new java.sql.Date(t)
    val r = sqlLiteral(format.format(d))
    r
  }

  private def sqlLiteral(d: Any): String = {
    "'" + sqlEscape(d.toString) + "'"
  }

  private def sqlEscape(s: String): String = {
    if (s.indexOf("'") == -1) s.replace("\\", "\\\\")
    else s.replace("'", "''").replace("\\", "\\\\")
  }

  private def sqlLiteralList(xs: Seq[String]): String =
    xs.map(sqlLiteral).mkString(", ")

  private def toSqlString(ts: Long): String = {
    val df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    df.setTimeZone(gmt)
    df.format(ts)
  }

  private def toSqlLiteral(dt: DateTime): String = toSqlLiteral(dt.getMillis)

  private def toSqlLiteral(ts: Long): String = "'" + toSqlString(ts) + "'"
}
