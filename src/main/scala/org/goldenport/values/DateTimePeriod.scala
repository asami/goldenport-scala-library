package org.goldenport.values

import scalaz._, Scalaz._
import scala.util.Try
import scala.util.control.NonFatal
import java.util.Date
import java.util.TimeZone
import java.text.SimpleDateFormat
import java.sql.Timestamp
import com.github.nscala_time.time.Imports._
import org.joda.time.DateTimeConstants._
import spire.math.Interval
import spire.math.interval._
import org.goldenport.RAISE
import org.goldenport.parser.ParseResult
import org.goldenport.context.DateTimeContext
import org.goldenport.collection.VectorMap
import org.goldenport.util.{DateTimeUtils, DateUtils, AnyUtils}
import org.goldenport.util.SpireUtils.Implicits._
import org.goldenport.values.Week._
import org.goldenport.values.IntervalFactory.{BoundsKind, CloseOpen, CloseClose}
import org.goldenport.extension.IRecord

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
 *  version Sep. 18, 2019
 *  version Sep. 10, 2020
 *  version Jan. 24, 2021
 *  version Feb. 28, 2021
 *  version Feb. 17, 2022
 *  version Apr. 21, 2022
 *  version May. 20, 2022
 *  version Jun. 17, 2022
 *  version Nov.  8, 2022
 * @version Feb. 24, 2023
 * @author  ASAMI, Tomoharu
 */
sealed trait DateTimePeriod { // TODO DateTimeInterval (java8 time)
  import DateTimePeriod._

  def start: Option[DateTime]
  def end: Option[DateTime]
  def isStartInclusive: Boolean //  = true
  def isEndInclusive: Boolean // = true

  def setStartEnd(s: DateTime, e: DateTime): DateTimePeriod
  def setStart(s: DateTime): DateTimePeriod
  def setEnd(e: DateTime): DateTimePeriod

  protected final def set_start_end(s: Option[DateTime], e: Option[DateTime]): DateTimePeriod =
    (s, e) match {
      case (Some(l), Some(r)) => setStartEnd(l, r)
      case (Some(l), None) => setStart(l)
      case (None, Some(r)) => setEnd(r)
      case (None, None) => DateTimePeriod.Whole
    }

  def toInterval: Interval[DateTime] = {
    val lower = _to_bound(start, isStartInclusive)
    val higher = _to_bound(end, isEndInclusive)
    Interval.fromBounds(lower, higher)
  }

  def toLocalDateTimeInterval: LocalDateTimeInterval = LocalDateTimeInterval(
    start.map(_.toLocalDateTime),
    end.map(_.toLocalDateTime),
    isStartInclusive,
    isEndInclusive
  )

  private def _to_bound(p: Option[DateTime], inclusive: Boolean): Bound[DateTime] =
    p.map(x => if (inclusive) Closed(x) else Open(x)).getOrElse(Unbound())

  def isValid(p: DateTime): Boolean = isValid(p.getMillis)

  def isValid(timestamp: Timestamp): Boolean = isValid(timestamp.getTime)

  def isAvaiableNow(): Boolean = isValid(DateTime.now)

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
      setEnd(DateTime.now(DateTimeUtils.jodajst))
      // copy(end = Some(DateTime.now(DateTimeUtils.jodajst))) // TODO TZ
  }

  def plusYears(n: Int): DateTimePeriod = {
    // copy(start = start.map(_.plusYears(n)), end = end.map(_.plusYears(n)))
    set_start_end(start.map(_.plusYears(n)), end.map(_.plusYears(n)))
  }

  def plusMonths(n: Int): DateTimePeriod = {
    // copy(start = start.map(_.plusMonths(n)), end = end.map(_.plusMonths(n)))
    set_start_end(start.map(_.plusMonths(n)), end.map(_.plusMonths(n)))
  }

  def plusDays(n: Int): DateTimePeriod = {
    // copy(start = start.map(_.plusDays(n)), end = end.map(_.plusDays(n)))
    set_start_end(start.map(_.plusDays(n)), end.map(_.plusDays(n)))
  }

  def minusYears(n: Int): DateTimePeriod = {
    // copy(start = start.map(_.minusYears(n)), end = end.map(_.minusYears(n)))
    set_start_end(start.map(_.minusYears(n)), end.map(_.minusYears(n)))
  }

  def minusMonths(n: Int): DateTimePeriod = {
    // copy(start = start.map(_.minusMonths(n)), end = end.map(_.minusMonths(n)))
    set_start_end(start.map(_.minusMonths(n)), end.map(_.minusMonths(n)))
  }

  def minusDays(n: Int): DateTimePeriod = {
    // copy(start = start.map(_.minusDays(n)), end = end.map(_.minusDays(n)))
    set_start_end(start.map(_.minusDays(n)), end.map(_.minusDays(n)))
  }

  // def intersect(rhs: DateTimePeriod): DateTimePeriod = {
  //   val s = (start, rhs.start) match {
  //     case (None, None) => None
  //     case (Some(l), None) => Some(l)
  //     case (None, Some(r)) => Some(r)
  //     case (Some(l), Some(r)) => {
  //       if (l.getMillis < r.getMillis) Some(r) else Some(l)
  //     }
  //   }
  //   val (e, i) = (end, rhs.end) match {
  //     case (None, None) => (None, isEndInclusive)
  //     case (Some(l), None) => (Some(l), isEndInclusive)
  //     case (None, Some(r)) => (Some(r), rhs.isEndInclusive)
  //     case (Some(l), Some(r)) => {
  //       if (l.getMillis == r.getMillis) {
  //         if (isEndInclusive) (Some(r), rhs.isEndInclusive)
  //         else (Some(l), isEndInclusive)
  //       } else if (l.getMillis < r.getMillis) (Some(l), isEndInclusive)
  //       else (Some(r), rhs.isEndInclusive)
  //     }
  //   }
  //   DateTimePeriod(s, e, i)
  // }

  // def intersect(rhs: DateTimePeriod): DateTimePeriod = {
  //   val si = (start, rhs.start) match {
  //     case (None, None) => (None, isStartInclusive && rhs.isStartInclusive)
  //     case (None, Some(r)) => (Some(r), rhs.isStartInclusive)
  //     case (Some(l), None) => (Some(l), isStartInclusive)
  //     case (Some(l), Some(r)) => ???
  //   }
  //   val ei = (end, rhs.end) match {
  //     case (None, None) => (None, isEndInclusive && rhs.isEndInclusive)
  //     case (None, Some(r)) => (Some(r), rhs.isEndInclusive)
  //     case (Some(l), None) => (Some(l), isEndInclusive)
  //     case (Some(l), Some(r)) => ???
  //   }
  //   (si._1, ei._1) match {
  //     case (Some(s), Some(e)) => ???
  //     case (Some(s), None) => DateTimePeriod(Some(s), None, si._2, ei._2)
  //     case (None, Some(e)) => DateTimePeriod(None, Some(e), si._2, ei._2)
  //     case (None, None) => Empty
  //   }
  // }

  def intersect(rhs: DateTimePeriod): DateTimePeriod =
    this match {
      case l: StartEnd => rhs match {
        case r: StartEnd => _intersect(l, r)
        case r: StartOnly => _intersect(l, r)
        case r: EndOnly => _intersect(l, r)
        case r: Just => _intersect(l, r)
        case Whole => l
        case Empty => Empty
      }
      case l: StartOnly => rhs match {
        case r: StartEnd => _intersect(r, l)
        case r: StartOnly => _intersect(l, r)
        case r: EndOnly => _intersect(l, r)
        case r: Just => _intersect(l, r)
        case Whole => l
        case Empty => Empty
      }
      case l: EndOnly => rhs match {
        case r: StartEnd => _intersect(r, l)
        case r: StartOnly => _intersect(r, l)
        case r: EndOnly => _intersect(l, r)
        case r: Just => _intersect(l, r)
        case Whole => l
        case Empty => Empty
      }
      case l: Just => rhs match {
        case r: StartEnd => _intersect(r, l)
        case r: StartOnly => _intersect(r, l)
        case r: EndOnly => _intersect(r, l)
        case r: Just => _intersect(l, r)
        case Whole => l
        case Empty => Empty
      }
      case Whole => rhs match {
        case r: StartEnd => r
        case r: StartOnly => r
        case r: EndOnly => r
        case r: Just => r
        case Whole => this
        case Empty => Empty
      }
      case Empty => rhs match {
        case r: StartEnd => this
        case r: StartOnly => this
        case r: EndOnly => this
        case r: Just => this
        case Whole => this
        case Empty => this
      }
    }

  private def _intersect(l: StartEnd, r: StartEnd): DateTimePeriod =
    if (l.startDateTime == r.endDateTime) {
      if (l.isStartInclusive && r.isEndInclusive)
        Just(l.startDateTime)
      else
        Empty
    } else if (r.startDateTime == l.endDateTime) {
      if (r.isStartInclusive && l.isEndInclusive)
        Just(r.startDateTime)
      else
        Empty
    } else if (l.startDateTime > r.endDateTime) {
      Empty
    } else {
      val s = DateTimeInclusive.max(l.startInclusive, r.startInclusive)
      val e = DateTimeInclusive.min(l.endInclusive, r.endInclusive)
      StartEnd(s, e)
    }

  private def _intersect(l: StartEnd, r: StartOnly): DateTimePeriod =
    l.setStart(DateTimeInclusive.max(l.startInclusive, r.startInclusive))

  private def _intersect(l: StartEnd, r: EndOnly): DateTimePeriod =
    l.setEnd(DateTimeInclusive.min(l.endInclusive, r.endInclusive))

  private def _intersect(l: StartEnd, r: Just): DateTimePeriod =
    if (l.isValid(r.dateTime))
      r
    else
      Empty

  private def _intersect(l: StartOnly, r: StartOnly): DateTimePeriod =
    l.setStart(DateTimeInclusive.max(l.startInclusive, r.startInclusive))

  private def _intersect(l: StartOnly, r: EndOnly): DateTimePeriod =
    if (l.startInclusive == r.endInclusive)
      Just(l.startDateTime)
    else if (l.startDateTime > r.endDateTime)
      Empty
    else
      StartEnd(l.startInclusive, r.endInclusive)

  private def _intersect(l: StartOnly, r: Just): DateTimePeriod =
    if (l.isValid(r.dateTime))
      r
    else
      Empty

  private def _intersect(l: EndOnly, r: EndOnly): DateTimePeriod =
    l.setEnd(DateTimeInclusive.min(l.endInclusive, r.endInclusive))

  private def _intersect(l: EndOnly, r: Just):DateTimePeriod =
    if (l.isValid(r.dateTime))
      r
    else
      Empty

  private def _intersect(l: Just, r: Just): DateTimePeriod =
    if (l.dateTime == r.dateTime)
      l
    else
      Empty

  def <(rhs: DateTimePeriod): Boolean =
    (start, end) match {
      case (None, None) => false
      case (_, Some(le)) => (rhs.start, rhs.end) match {
        case (None, _) => false
        case (Some(rs), _) => (isEndInclusive, rhs.isStartInclusive) match {
          case (true, true) => le < rs
          case _ => le <= rs
        }
      }
      case (Some(ls), _) => false
    }

  def >(rhs: DateTimePeriod): Boolean =
    (start, end) match {
      case (None, None) => false
      case (Some(ls), _) => (rhs.start, rhs.end) match {
        case (_, None) => false
        case (_, Some(re)) => (isStartInclusive, rhs.isEndInclusive) match {
          case (true, true) => re < ls
          case _ => re <= ls
        }
      }
      case (_, Some(le)) => false
    }

  def isConflict(rhs: DateTimePeriod): Boolean = !(<(rhs) || >(rhs))

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

  def literal(): String = {
    def prefix = if (isStartInclusive) MARK_START_CLOSE else MARK_START_OPEN
    def postfix = if (isEndInclusive) MARK_END_CLOSE else MARK_END_OPEN
    val s = start match {
      case Some(s) => prefix + DateTimeUtils.toIsoDateTimeString(s)
      case None => ""
    }
    val e = end match {
      case Some(s) => DateTimeUtils.toIsoDateTimeString(s) + postfix
      case None => ""
    }
    s + "~" + e
  }

  def literal(ctx: DateTimeContext): String = {
    def prefix = if (isStartInclusive) MARK_START_CLOSE else MARK_START_OPEN
    def postfix = if (isEndInclusive) MARK_END_CLOSE else MARK_END_OPEN
    val s = start match {
      case Some(s) => prefix + DateTimeUtils.toIsoDateTimeString(s)
      case None => ""
    }
    val e = end match {
      case Some(s) => DateTimeUtils.toIsoDateTimeString(s) + postfix
      case None => ""
    }
    s + "~" + e
  }

  def toNaturalJst: String = {
    DateTimeUtils.toNaturalStringJst(start, end, isEndInclusive)
  }

  def toPrehistory: DateTimePeriod = {
    start match {
      case Some(s) => DateTimePeriod.EndOnly(start, false)
      case None => DateTimePeriod.empty
    }
  }

  def toRecord(): IRecord = {
    val t = Vector("text" -> literal())
    val s = start match {
      case Some(s) =>
        val r = DateTimeUtils.toRecord(s) + IRecord.data(
          "datetime" -> DateTimeUtils.toIsoDateTimeString(s),
          "inclusive" -> isStartInclusive
        )
        Vector("start" -> r)
      case None => Vector.empty
    }
    val e = end match {
      case Some(s) =>
        val r = DateTimeUtils.toRecord(s) + IRecord.data(
          "datetime" -> DateTimeUtils.toIsoDateTimeString(s),
          "inclusive" -> isEndInclusive
        )
        Vector("end" -> r)
      case None => Vector.empty
    }
    IRecord.create(t ++ s ++ e)
  }

  def toRecord(ctx: DateTimeContext): IRecord = {
    val t = Vector("text" -> literal(ctx))
    val s = start match {
      case Some(s) =>
        val r = DateTimeUtils.toRecord(s, ctx) + IRecord.data(
          "inclusive" -> isStartInclusive
        )
        Vector("start" -> r)
      case None => Vector.empty
    }
    val e = end match {
      case Some(s) =>
        val r = DateTimeUtils.toRecord(s, ctx) + IRecord.data(
          "inclusive" -> isEndInclusive
        )
        Vector("end" -> r)
      case None => Vector.empty
    }
    IRecord.create(t ++ s ++ e)
  }
}

object DateTimePeriod {
  implicit object DateTimePeriodMonoid extends Monoid[DateTimePeriod] {
    def append(lhs: DateTimePeriod, rhs: => DateTimePeriod) = lhs intersect rhs
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

  case class DateTimeInclusive(dateTime: DateTime, isInclusive: Boolean = true) {
    def set(p: DateTime) = copy(dateTime = p)
  }
  object DateTimeInclusive {
    def max(l: DateTimeInclusive, r: DateTimeInclusive): DateTimeInclusive =
      if (l.dateTime == r.dateTime) {
        if (l.isInclusive)
          r
        else
          l
      } else if (l.dateTime > r.dateTime) {
        l
      } else {
        r
      }

    def min(l: DateTimeInclusive, r: DateTimeInclusive): DateTimeInclusive = 
      if (l.dateTime == r.dateTime) {
        if (l.isInclusive)
          r
        else
          l
      } else if (l.dateTime > r.dateTime) {
        r
      } else {
        l
      }
  }

  case class StartEnd(
    startInclusive: DateTimeInclusive,
    endInclusive: DateTimeInclusive
  ) extends DateTimePeriod {
    def startDateTime = startInclusive.dateTime
    def endDateTime = endInclusive.dateTime
    val start: Option[DateTime] = Some(startInclusive.dateTime)
    val end: Option[DateTime] = Some(endInclusive.dateTime)
    def isStartInclusive: Boolean = startInclusive.isInclusive
    def isEndInclusive: Boolean = endInclusive.isInclusive

    def setStartEnd(s: DateTime, e: DateTime): DateTimePeriod =
      copy(startInclusive = startInclusive.set(s), endInclusive = endInclusive.set(e))
    def setStart(s: DateTime): DateTimePeriod =
      copy(startInclusive = startInclusive.set(s))
    def setEnd(e: DateTime): DateTimePeriod =
      copy(endInclusive = endInclusive.set(e))

    def setStart(p: DateTimeInclusive) = copy(startInclusive = p)
    def setEnd(p: DateTimeInclusive) = copy(endInclusive = p)
  }
  object StartEnd {
    def apply(
      s: DateTime,
      e: DateTime,
      si: Boolean,
      ei: Boolean
    ): StartEnd = StartEnd(DateTimeInclusive(s, si), DateTimeInclusive(e, ei))

    def apply(
      s: DateTime,
      e: DateTime
    ): StartEnd = StartEnd(DateTimeInclusive(s), DateTimeInclusive(e))
  }

  case class StartOnly(
    startInclusive: DateTimeInclusive
  ) extends DateTimePeriod {
    def startDateTime = startInclusive.dateTime
    val start: Option[DateTime] = Some(startInclusive.dateTime)
    val end: Option[DateTime] = None
    def isStartInclusive: Boolean = startInclusive.isInclusive
    def isEndInclusive: Boolean = true

    def setStartEnd(s: DateTime, e: DateTime): DateTimePeriod =
      StartEnd(s, e, isStartInclusive, isEndInclusive)
    def setStart(s: DateTime): DateTimePeriod =
      copy(startInclusive = startInclusive.set(s))
    def setEnd(e: DateTime): DateTimePeriod =
      StartEnd(startInclusive, DateTimeInclusive(e))

    def setStart(p: DateTimeInclusive) = copy(startInclusive = p)
  }
  object StartOnly {
    def apply(
      s: DateTime,
      si: Boolean
    ): StartOnly = StartOnly(DateTimeInclusive(s, si))

    def apply(
      s: DateTime
    ): StartOnly = StartOnly(DateTimeInclusive(s))
  }

  case class EndOnly (
    endInclusive: DateTimeInclusive
  ) extends DateTimePeriod {
    def endDateTime = endInclusive.dateTime
    val start: Option[DateTime] = None
    val end: Option[DateTime] = Some(endInclusive.dateTime)
    def isStartInclusive: Boolean = true
    def isEndInclusive: Boolean = endInclusive.isInclusive

    def setStartEnd(s: DateTime, e: DateTime): DateTimePeriod =
      StartEnd(s, e, isStartInclusive, isEndInclusive)
    def setStart(s: DateTime): DateTimePeriod =
      StartEnd(DateTimeInclusive(s, isStartInclusive), endInclusive)
    def setEnd(e: DateTime): DateTimePeriod =
      copy(endInclusive = endInclusive.set(e))

    def setEnd(p: DateTimeInclusive) = copy(endInclusive = p)
  }
  object EndOnly {
    def apply(
      e: DateTime,
      ei: Boolean
    ): EndOnly = EndOnly(DateTimeInclusive(e, ei))

    def apply(
      e: Option[DateTime],
      ei: Boolean
    ): DateTimePeriod = e.map(x => EndOnly(DateTimeInclusive(x, ei))).getOrElse(Whole)

    def apply(
      e: DateTime
    ): EndOnly = EndOnly(DateTimeInclusive(e))
  }

  case class Just(
    dateTime: DateTime
  ) extends DateTimePeriod {
    val start: Option[DateTime] = Some(dateTime)
    val end: Option[DateTime] = Some(dateTime)
    def isStartInclusive: Boolean = true
    def isEndInclusive: Boolean = true

    def setStartEnd(s: DateTime, e: DateTime): DateTimePeriod =
      if (s <= dateTime && dateTime <= e)
        this
      else
        Empty
    def setStart(s: DateTime): DateTimePeriod =
      if (s == dateTime)
        this
      else
        Empty
    def setEnd(e: DateTime): DateTimePeriod =
      if (e == dateTime)
        this
      else
        Empty
  }

  case object Whole extends DateTimePeriod {
    val start: Option[DateTime] = None
    val end: Option[DateTime] = None
    def isStartInclusive: Boolean = true
    def isEndInclusive: Boolean = true

    def setStartEnd(s: DateTime, e: DateTime): DateTimePeriod =
      StartEnd(s, e, isStartInclusive, isEndInclusive)
    def setStart(s: DateTime): DateTimePeriod =
      StartOnly(s, isStartInclusive)
    def setEnd(e: DateTime): DateTimePeriod =
      EndOnly(e, isEndInclusive)
  }

  case object Empty extends DateTimePeriod {
    val start: Option[DateTime] = None
    val end: Option[DateTime] = None
    def isStartInclusive: Boolean = true
    def isEndInclusive: Boolean = true

    def setStartEnd(s: DateTime, e: DateTime): DateTimePeriod = this
    def setStart(s: DateTime): DateTimePeriod = this
    def setEnd(e: DateTime): DateTimePeriod = this
  }

  val empty = Empty

  val THREASHOLD_DAY = 1
  val THREASHOLD_HOUR = 6

  def apply(s: Option[DateTime], e: Option[DateTime], si: Boolean, ei: Boolean): DateTimePeriod =
    (s, e) match {
      case (Some(l), Some(r)) =>
        if (l == r)
          if (si && ei) {
            Just(l)
          } else {
            Empty
          }
        else
          StartEnd(l, r, si, ei)
      case (Some(l), None) =>
        StartOnly(l, si)
      case (None, Some(r)) =>
        EndOnly(r, ei)
      case (None, None) =>
        Whole
    }

  def apply(s: Option[DateTime], e: Option[DateTime]): DateTimePeriod =
    (s, e) match {
      case (Some(l), Some(r)) => StartEnd(l, r)
      case (Some(l), None) => StartOnly(l)
      case (None, Some(r)) => EndOnly(r)
      case (None, None) => Whole
    }

  private def effectiveYearMonth(year: Int, month: Int, day: Int): (Int, Int) = {
    if (day <= THREASHOLD_DAY) {
      if (month == 1)
        (year - 1, 12)
      else
        (year, month - 1)
    } else
      (year, month)
  }

  private def effectiveYearMonthDay(tz: DateTimeZone)(year: Int, month: Int, day: Int, hour: Int): (Int, Int, Int) = {
    val dt = if (hour <= THREASHOLD_HOUR) {
      new DateTime(year, month, day, 0, 0, tz).
        minusDays(2)
    } else {
      new DateTime(year, month, day, 0, 0, tz).
        minusDays(1)
    }
    (dt.getYear, dt.getMonthOfYear, dt.getDayOfMonth)
  }

  def inclusiveExclusive(start: DateTime, end: DateTime): DateTimePeriod =
    DateTimePeriod(Some(start), Some(end), true, false)

  def closeOpen(start: DateTime, end: DateTime): DateTimePeriod =
    DateTimePeriod(Some(start), Some(end), true, false)

  def closeOpen(start: Timestamp, end: Timestamp, tz: TimeZone): DateTimePeriod = {
    val s = DateTimeUtils.toDateTime(start, tz)
    val e = DateTimeUtils.toDateTime(end, tz)
    closeOpen(s, e)
  }

  def atOrAbove(year: Int, month: Int, day: Int, hour: Int, minute:Int, second:Int, tz: DateTimeZone): DateTimePeriod =
    DateTimePeriod(Some(new DateTime(year, month, day, hour, minute, second, tz)), None)

  def atOrAboveJst(year: Int, month: Int, day: Int): DateTimePeriod =
    DateTimePeriod(Some(new DateTime(year, month, day, 0, 0, jodajst)), None)


  def thisMonth(p: DateTime): DateTimePeriod = {
    val start = DateTimeUtils.startOfFirstDayOfThisMonth(p)
    val end = DateTimeUtils.startOfFirstDayOfNextMonth(p)
    DateTimePeriod.inclusiveExclusive(start, end)
  }

  def lastOneMonthDayBoundary(p: DateTime): DateTimePeriod = {
    val end = DateTimeUtils.startOfToday(p)
    val start = end.minusMonths(1)
    DateTimePeriod.inclusiveExclusive(start, end)
  }

  /*
   * Caution: Timezone depends running environment.
   */
  def parse(p: String): DateTimePeriod = parse(DateTime.now, p)

  def parse(now: DateTime, p: String): DateTimePeriod = parse(now, now.getZone, p)

  def parse(tz: DateTimeZone, p: String): DateTimePeriod = parse(DateTime.now, tz, p)

  def parse(now: DateTime, tz: DateTimeZone, p: String): DateTimePeriod =
    parse(DateTimeContext(now, tz), p)

  def parse(ctx: DateTimeContext, p: String): DateTimePeriod =
    Builder(ctx).fromExpression(p)

  def parseJst(p: String): DateTimePeriod = parse(jodajst, p)

  def parseCloseOpenJst(p: String): DateTimePeriod = parse(CloseOpen, jodajst, p)

  def parse(kind: BoundsKind, tz: DateTimeZone, p: String): DateTimePeriod = parse(kind, DateTime.now, tz, p)

  def parse(
    kind: BoundsKind,
    now: DateTime,
    tz: DateTimeZone,
    p: String
  ): DateTimePeriod = Builder(DateTimeContext(now, tz), kind).fromExpression(p)

  def parseNonEmpty(ctx: DateTimeContext, p: String): DateTimePeriod =
    Builder(ctx, acceptEmpty = false, acceptJust = false).fromExpression(p)

  def parseFill(ctx: DateTimeContext, p: String): DateTimePeriod =
    Builder(ctx, acceptEmpty = false, acceptJust = false, acceptEmptyStart = false, acceptEmptyEnd = false).fromExpression(p)

  /*
   * Caution: Timezone depends running environment.
   */
  def parseOption(p: String): Option[DateTimePeriod] = Try(parse(p)).toOption

  case class Builder(
    context: DateTimeContext,
    kind: BoundsKind = CloseClose,
    acceptEmpty: Boolean = true,
    acceptJust: Boolean = true,
    acceptEmptyStart: Boolean = true,
    acceptEmptyEnd: Boolean = true
  ) {
    import RichDateTime.Implicits._

    def isStartInclusive: Boolean = kind.isStartInclusive
    def isEndInclusive: Boolean = kind.isEndInclusive

    val datetime: DateTime = context.current
    def currentYear: Int = datetime.currentYear
    def currentMonth: Int = datetime.currentMonth 
    def currentWeek: Int = datetime.currentWeek
    def currentDay: Int = datetime.currentDay
    def currentHour: Int = datetime.currentHour
    def dateTimeZone: DateTimeZone = context.dateTimeZone

    def create(
      start: Option[String],
      end: Option[String]
    ): DateTimePeriod = create(start, end, isEndInclusive)

    def create(
      start: Option[String],
      end: Option[String],
      inclusive: Boolean
    ): DateTimePeriod = {
      _create(
        start.map(toDateTime),
        end.map(toDateTime),
        isStartInclusive,
        inclusive
      )
    }

    def create(
      start: Option[String],
      end: Option[String],
      low: Boolean,
      high: Boolean
    ): DateTimePeriod = {
      _create(
        start.map(toDateTime),
        end.map(toDateTime),
        low,
        high
      )
    }

    private def _create(
      s: Option[DateTime],
      e: Option[DateTime]
    ): DateTimePeriod = _create(s, e, isStartInclusive, isEndInclusive)

    private def _create(
      s: Option[DateTime],
      e: Option[DateTime],
      si: Boolean,
      ei: Boolean
    ): DateTimePeriod = {
      val a = DateTimePeriod(s, e, si, ei)
      _check(a) match {
        case Some(error) => RAISE.invalidArgumentFault(error)
        case None => a
      }
    }

    private def _check(p: DateTimePeriod): Option[String] = {
      val a = if (!acceptEmpty && p == Empty)
        Some("Empty interval")
      else if (!acceptJust && p.isInstanceOf[Just])
        Some("Just interval")
      else
        (acceptEmptyStart, acceptEmptyEnd) match {
          case (true, true) => None
          case (true, false) =>
            if (p.end.isEmpty)
              Some("Missing end.")
            else
              None
          case (false, true) =>
            if (p.start.isEmpty)
              Some("Missing start.")
            else
              None
          case (false, false) =>
            if (p.start.isEmpty && p.end.isEmpty)
              Some("Missing both start and end.")
            else
              None
        }
      a orElse {
        p match {
          case StartEnd(s, e) =>
            if (s.dateTime > e.dateTime)
              Some("Start is later than end.")
            else
              None
          case _ => None
        }
      }
    }

    def toDateTime(
      s: String
    ): DateTime = {
      def plain(a: String) = a match {
        case YearMonthDay(y, m, d) =>
          new DateTime(y.toInt, m.toInt, d.toInt, 0, 0, dateTimeZone)
        case YearMonth(y, m) =>
          new DateTime(y.toInt, m.toInt, 1, 0, 0, dateTimeZone)
        case Year(y) =>
          new DateTime(y.toInt, 1, 1, 0, 0, dateTimeZone)
        case _ => datetimeParse(a)
      }
      def plus(a: String, n: Int) = a match {
        case YearMonthDay(y, m, d) =>
          new DateTime(y.toInt, m.toInt, d.toInt, 0, 0, dateTimeZone).plusDays(n)
        case YearMonth(y, m) =>
          new DateTime(y.toInt, m.toInt, 1, 0, 0, dateTimeZone).plusMonths(n)
        case Year(y) =>
          new DateTime(y.toInt, 1, 1, 0, 0, dateTimeZone).plusYears(n)
      }
      def minus(a: String, n: Int) = a match {
        case YearMonthDay(y, m, d) =>
          new DateTime(y.toInt, m.toInt, d.toInt, 0, 0, dateTimeZone).minusDays(n)
        case YearMonth(y, m) =>
          new DateTime(y.toInt, m.toInt, 1, 0, 0, dateTimeZone).minusMonths(n)
        case Year(y) =>
          new DateTime(y.toInt, 1, 1, 0, 0, dateTimeZone).minusYears(n)
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
        _create(
          Some(tz.yearFirst(y)),
          Some(tz.yearLast(y)))
      }
      def yearmonth(y: Int, m: Int) = {
        _create(
          Some(tz.monthFirst(y, m)),
          Some(tz.monthLast(y, m)))
      }
      def yearmonthday(y: Int, m: Int, d: Int) = {
        _create(
          Some(tz.dayFirst(y, m, d)),
          Some(tz.dayLast(y, m, d)))
      }

      def plain_one(s: String): DateTimePeriod = s match {
        case "year" => year(base.getYear)
        case "month" => yearmonth(base.getYear, base.getMonthOfYear)
        case "day" => yearmonthday(base.getYear, base.getMonthOfYear, base.getDayOfMonth)
        case KEY_DAY_PRESISE_TIME => _create(Some(base), Some(base.plusDays(1).minusMillis(1)))
        case Year(y) => year(y.toInt)
        case YearMonth(y, m) => yearmonth(y.toInt, m.toInt)
        case YearMonthDay(y, m, d) => yearmonthday(y.toInt, m.toInt, d.toInt)
        case _ => throw new IllegalArgumentException(s"Invalid period: $s")
      }
      def plus_one(s: String, n: Int): DateTimePeriod = s match {
        case "year" => year(base.getYear).plusYears(n)
        case "month" => yearmonth(base.getYear, base.getMonthOfYear).plusMonths(n)
        case "day" => yearmonthday(base.getYear, base.getMonthOfYear, base.getDayOfMonth).plusDays(n)
        case KEY_DAY_PRESISE_TIME => _create(Some(base), Some(base.plusDays(n).minusMillis(1)))
        case Year(y) => year(y.toInt).plusYears(n)
        case YearMonth(y, m) => yearmonth(y.toInt, m.toInt).plusMonths(n)
        case YearMonthDay(y, m, d) => yearmonthday(y.toInt, m.toInt, d.toInt).plusDays(n)
        case _ => throw new IllegalArgumentException(s"Invalid period: $s, $n")
      }
      def minus_one(s: String, n: Int): DateTimePeriod = s match {
        case "year" => year(base.getYear).minusYears(n)
        case "month" => yearmonth(base.getYear, base.getMonthOfYear).minusMonths(n)
        case "day" => yearmonthday(base.getYear, base.getMonthOfYear, base.getDayOfMonth).minusDays(n)
        case KEY_DAY_PRESISE_TIME => _create(Some(base.minusDays(n)), Some(base.minusDays(n - 1).minusMillis(1)))
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

      def body(inclusivep: Boolean, sb: String, si: Boolean, ei: Boolean): DateTimePeriod = {
//        println(s"body: $inclusivep, $sb, $si, $ei")
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
          DateTimePeriod(s1, e1, si, ei)
        }
      }

      if (s.isEmpty) {
        RAISE.invalidArgumentFault("Empty datetime")
      } else {
        val (a, isstartinclusive) = s.head match {
          case MARK_START_OPEN => (s.tail, false)
          case MARK_START_CLOSE => (s.tail, true)
          case _ => (s, isStartInclusive)
        }
        val (b, isendinclusive) = a.last match {
          case MARK_END_OPEN => (a.init, false)
          case MARK_END_CLOSE => (a.init, true)
          case MARK_OPEN => (a.init, false)
          case _ => (a, isEndInclusive)
        }
//        println(s"pre body: $s, $a, $b)")
        val r = body(false, b, isstartinclusive, isendinclusive)
        _check(r) match {
          case Some(error) => RAISE.invalidArgumentFault(s"$error: $s")
          case None => r
        }
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
      _create(
        Some(datetimeYearFirst(year)),
        Some(datetimeYearLast(year)))
    }

    def monthly: DateTimePeriod = {
      monthly(currentYear, currentMonth)
    }

    def effectiveMonthly: DateTimePeriod = {
      val (year, month) = effectiveYearMonth(currentYear, currentMonth, currentDay)
      val start = new DateTime(year, month, 1, 0, 0, dateTimeZone)
      val end = new DateTime(year, month, 1, 0, 0, dateTimeZone).plusMonths(1).minusDays(1)
      _create(Some(start), Some(end))
    }

    def monthly(
      year: Int, month: Int
    ): DateTimePeriod = {
      _create(
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
      _create(
        Some(datetimeWeekFirst(year, week)),
        Some(datetimeWeekLast(year, week)))
    }

    def daily: DateTimePeriod = {
      daily(currentYear, currentMonth, currentDay)
    }

    def effectiveDaily: DateTimePeriod = {
      val (year, month, day) = effectiveYearMonthDay(dateTimeZone)(currentYear, currentMonth, currentDay, currentHour)
      val start = new DateTime(year, month, day, 0, 0, dateTimeZone)
      val end = new DateTime(year, month, day, 0, 0, dateTimeZone).plusDays(1).minusMillis(1)
      _create(Some(start), Some(end))
    }

    def daily(
      year: Int, month: Int, day: Int
    ): DateTimePeriod = {
      _create(
        Some(datetimeDayFirst(year, month, day)),
        Some(datetimeDayLast(year, month, day)))
    }

    def datetimeYearFirst(year: Int): DateTime = {
      new DateTime(year, 1, 1, 0, 0, dateTimeZone)
    }

    def datetimeYearLast(year: Int): DateTime = {
      val dt = new DateTime(year, 1, 1, 0, 0, dateTimeZone)
      dt.plusYears(1).minusMillis(1)
    }

    def datetimeMonthFirst(year: Int, month: Int): DateTime = {
      new DateTime(year, month, 1, 0, 0, dateTimeZone)
    }

    def datetimeMonthLast(year: Int, month: Int): DateTime = {
      val dt = new DateTime(year, month, 1, 0, 0, dateTimeZone)
      dt.plusMonths(1).minusMillis(1)
    }

    def datetimeWeekFirst(year: Int, week: Int): DateTime = {
      RAISE.notImplementedYetDefect
    }

    def datetimeWeekLast(year: Int, week: Int): DateTime = {
      // val dt = new DateTime(year, month, 1, 0, 0, dateTimeZone)
      // dt.plusMonths(1).minusMillis(1)
      RAISE.notImplementedYetDefect
    }

    def datetimeDayFirst(year: Int, month: Int, day: Int): DateTime = {
      new DateTime(year, month, day, 0, 0, dateTimeZone)
    }

    def datetimeDayLast(year: Int, month: Int, day: Int): DateTime = {
      val dt = new DateTime(year, month, day, 0, 0, dateTimeZone)
      dt.plusDays(1).minusMillis(1)
    }

    def datetimeParse(s: String): DateTime = parseDateTime(s, dateTimeZone)

    def parseDateTime(s: String, tz: DateTimeZone): DateTime =
      DateTimeUtils.parseDateTime(s, tz)
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
