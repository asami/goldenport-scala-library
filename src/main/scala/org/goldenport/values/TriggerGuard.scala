package org.goldenport.values

import scalaz._, Scalaz._
import scala.util.control.NonFatal
import java.sql.Timestamp
import com.github.nscala_time.time.Imports._
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.context.DateTimeContext
import org.goldenport.parser._
import org.goldenport.util.DateTimeUtils

/*
 * @since   Feb. 14, 2021
 * @version Feb. 28, 2021
 * @author  ASAMI, Tomoharu
 */
sealed trait TriggerGuard {
}

object TriggerGuard {
}

case class CronTriggerGuard(
  minute: CronTriggerGuard.MinuteGuard,
  hour: CronTriggerGuard.HourGuard,
  day: CronTriggerGuard.DayGuard,
  month: CronTriggerGuard.MonthGuard,
  dayOfWeek: CronTriggerGuard.DayOfWeekGuard,
  accuracy: Duration,
  timezone: DateTimeZone
) extends TriggerGuard {
  import CronTriggerGuard._

  def getTimestamp: Option[Timestamp] = {
    val now = DateTime.now(timezone)
    _get_timestamp(now)
  }

  def getTimestamp(ctx: DateTimeContext): Option[Timestamp] = {
    _get_timestamp(ctx.current)
  }

  def getTimestamp(now: DateTime): Option[Timestamp] =
    _get_timestamp(DateTimeUtils.toDateTime(now, timezone))

  private def _get_timestamp(now: DateTime): Option[Timestamp] = {
    if (dayOfWeek.isActive)
      _to_timestamp_weekly(now, _is_valid)
    else if (month.isActive)
      _to_timestamp_absolute_day(now, _is_valid)
    else if (day.isActive)
      _to_timestamp_monthly(now, now, _is_valid)
    else if (hour.isActive)
      _to_timestamp_daily(now, _is_valid)
    else if (minute.isActive)
      _to_timestamp_every_hour(now, _is_valid)
    else
      _to_timestamp_every_minute(now, _is_valid)
  }

  private def _is_valid(dt: DateTime, accuracy: Duration, now: DateTime) = {
    val a = now >= dt 
    val base = now - accuracy
    val b = dt >= base
    a & b
  }

  def getTimestamp(now: DateTime, leadtime: Long): Option[ActionPrepareAt] = {
    val a = DateTimeUtils.toDateTime(now, timezone)
    val candidate = a + leadtime
    _get_timestamp(a, candidate).map(x =>
      ActionPrepareAt(x, new Timestamp(x.getTime - leadtime))
    )
  }

  private def _is_valid_leadtime(
    candidate: DateTime
  )(dt: DateTime, accuracy: Duration, now: DateTime) = {
    (candidate >= dt && dt >= now) || _is_valid(dt, accuracy, now)
  }

  private def _get_timestamp(now: DateTime, candidate: DateTime): Option[Timestamp] = {
    if (dayOfWeek.isActive)
      _to_timestamp_weekly(now, _is_valid_leadtime(candidate))
    else if (month.isActive)
      _to_timestamp_absolute_day(now, _is_valid_leadtime(candidate))
    else if (day.isActive)
      _to_timestamp_monthly(now, candidate, _is_valid_leadtime(candidate))
    else if (hour.isActive)
      _to_timestamp_daily(now, _is_valid_leadtime(candidate))
    else if (minute.isActive)
      _to_timestamp_every_hour(now, _is_valid_leadtime(candidate))
    else
      _to_timestamp_every_minute(now, _is_valid_leadtime(candidate))
  }

  private def _to_timestamp_weekly(
    now: DateTime,
    predicate: (DateTime, Duration, DateTime) => Boolean
  ): Option[Timestamp] = {
    val a = for {
      (ys, mos, ds) <- _weekly_candidates(now)
      hs <- hour.candidates
      ms <- minute.candidates
      dt = new DateTime(ys, mos, ds, hs, ms, timezone)
      if predicate(dt, accuracy, now)
    } yield dt
    a.nonEmpty option {
      val b = a.maxBy(_.getMillis)
      new Timestamp(b.getMillis)
    }
  }

  private def _to_timestamp_absolute_day(
    now: DateTime,
    predicate: (DateTime, Duration, DateTime) => Boolean
  ) = {
    val a = for {
      ys <- _year_candidates(now)
      mos <- month.candidates
      ds <- day.candidates
      hs <- hour.candidates
      ms <- minute.candidates
      dt = new DateTime(ys, mos, ds, hs, ms, timezone)
      if predicate(dt, accuracy, now)
    } yield dt
    a.nonEmpty option {
      val b = a.maxBy(_.getMillis)
      new Timestamp(b.getMillis)
    }
  }

  private def _to_timestamp_monthly(
    now: DateTime,
    candidate: DateTime,
    predicate: (DateTime, Duration, DateTime) => Boolean
  ) = {
    val a = for {
      ys <- _year_candidates(candidate)
      mos <- _month_candidates(candidate)
      ds <- day.candidates
      hs <- hour.candidates
      ms <- minute.candidates
      dt = new DateTime(ys, mos, ds, hs, ms, timezone)
      if predicate(dt, accuracy, now)
    } yield dt
    a.nonEmpty option {
      val b = a.maxBy(_.getMillis)
      new Timestamp(b.getMillis)
    }
  }

  private def _to_timestamp_daily(
    now: DateTime,
    predicate: (DateTime, Duration, DateTime) => Boolean
  ) = {
    val a = for {
      ys <- _year_candidates(now)
      mos <- _month_candidates(now)
      ds <- _day_candidates(now)
      hs <- hour.candidates
      ms <- minute.candidates
      dt = new DateTime(ys, mos, ds, hs, ms, timezone)
      if predicate(dt, accuracy, now)
    } yield dt
    a.nonEmpty option {
      val b = a.maxBy(_.getMillis)
      new Timestamp(b.getMillis)
    }
  }

  private def _to_timestamp_every_hour(
    now: DateTime,
    predicate: (DateTime, Duration, DateTime) => Boolean
  ) = {
    val a = for {
      ms <- minute.candidates
      dt = now.withMinuteOfHour(ms).withSecondOfMinute(0).withMillisOfSecond(0)
      if predicate(dt, accuracy, now)
    } yield dt
    a.nonEmpty option {
      val b = a.maxBy(_.getMillis)
      new Timestamp(b.getMillis)
    }
  }

  private def _to_timestamp_every_minute(
    now: DateTime,
    predicate: (DateTime, Duration, DateTime) => Boolean
  ) = {
    val a = for {
      dt <- Some(now.withSecondOfMinute(0).withMillisOfSecond(0))
      if predicate(dt, accuracy, now)
    } yield dt
    a.nonEmpty option {
      val b = a.maxBy(_.getMillis)
      new Timestamp(b.getMillis)
    }
  }

  private def _year_candidates(now: DateTime): List[Int] = {
    if (now.getMonthOfYear == 12)
      List(now.getYear, now.getYear - 1)
    else
      List(now.getYear)
  } 

  private def _month_candidates(now: DateTime): List[Int] = {
    if (now.getDayOfMonth == 1)
      List(now.getMonthOfYear, _previous_month(now))
    else
      List(now.getMonthOfYear)
  } 

  private def _previous_month(now: DateTime) = DateTimeUtils.asPreviousMonth(now)

  private def _day_candidates(now: DateTime): List[Int] = {
    if (now.getHourOfDay == 0)
      List(now.getDayOfMonth, _previous_day(now))
    else
      List(now.getDayOfMonth)
  } 

  private def _previous_day(now: DateTime) = DateTimeUtils.asPreviousDay(now)

  private def _weekly_candidates(now: DateTime): List[(Int, Int, Int)] = {
    val a = _weekly_candidates_specific(now)
    val b = _weekly_candidates_biweek(now)
    val c = _weekly_candidates_weeknumbers(now)
    // println(s"_weekly_candidates: $a")
    // println(s"_weekly_candidates: $b")
    // println(s"_weekly_candidates: $c")
    // println(s"c: ${dayOfWeek.values}")
    a ++ b ++ c
  }

  private def _weekly_candidates_specific(now: DateTime): List[(Int, Int, Int)] = {
    val start = (now + 1.week)
    val xs = dayOfWeek.specificCandidates.map(start.withDayOfWeek)
    val r = for {
      i <- 0 to 5
      x <- xs
    } yield {
      val y = x - i.week
      (y.getYear, y.getMonthOfYear, y.getDayOfMonth)
    }
    r.toList
  }

  private def _weekly_candidates_biweek(now: DateTime): List[(Int, Int, Int)] =
    dayOfWeek.biweeks.flatMap(_.candidates(now)).map(_normalize)

  private def _weekly_candidates_weeknumbers(now: DateTime): List[(Int, Int, Int)] =
    dayOfWeek.weeknumbers.flatMap(_.candidates(now)).map(_normalize)

  private def _normalize(p: DateTime) = (p.getYear, p.getMonthOfYear, p.getDayOfMonth)
}

object CronTriggerGuard {
  sealed trait Expression
  sealed trait IntExpression extends Expression {
    def candidates: List[Int]
  }
  case class IntValueExpression(value: Int) extends IntExpression {
    require (value >= 0, "Value should be greater than 0")
    require (value <= 60, "Value should be lesser than 60")
    def candidates = List(value)
  }
  case class IntRangeExpression(start: Int, end: Int) extends IntExpression {
    // Inclusive
    require (start >= 0, "Start should be greater than 0")
    require (start <= 60, "Start should be lesser than 60")
    require (end >= 0, "End should be greater than 0")
    require (end <= 60, "End should be lesser than 60")
    require (start > end, "Start should be greater than end")
    def candidates = RAISE.notImplementedYetDefect
  }
  sealed trait DayOfWeekExpression extends Expression {
    protected final def is_in_scope(now: DateTime, p: DateTime): Boolean = {
      val start = now.minusDays(14)
      val end = now.plusDays(1)
      start <= p && p <= end
    }

    protected final def is_out_of_scope(now: DateTime, p: DateTime): Boolean =
      now.plusDays(1) < p
  }
  case class SyntaxErrorDayOfWeekExpression(v: String) extends DayOfWeekExpression {
  }
  sealed trait SpecificDayOfWeekExpression extends DayOfWeekExpression {
    def toInt: Int // DateTime#withDayOfWeek
  }
  case object Sunday extends SpecificDayOfWeekExpression { // Sun. 0, 7
    def toInt = 7
  }
  case object Monday extends SpecificDayOfWeekExpression { // Mon. 1
    def toInt = 1
  }
  case object Tuesday extends SpecificDayOfWeekExpression { // Tue. Tues. 2
    def toInt = 2
  }
  case object Wednesday extends SpecificDayOfWeekExpression { // Wed. 3
    def toInt = 3
  }
  case object Thursday extends SpecificDayOfWeekExpression { // Thurs. 4
    def toInt = 4
  }
  case object Friday extends SpecificDayOfWeekExpression { // Fri. 5
    def toInt = 5
  }
  case object Saturday extends SpecificDayOfWeekExpression { // Sat. 6
    def toInt = 6
  }
  case class DayOfWeekNumber(
    dow: SpecificDayOfWeekExpression,
    first: Boolean,
    second: Boolean,
    third: Boolean,
    fourth: Boolean,
    fifth: Boolean
  ) extends DayOfWeekExpression {
    def candidates(now: DateTime): Vector[DateTime] = {
      val start = now.minusMonths(1).withDayOfMonth(1)
      case class Z(
        r: Vector[DateTime] = Vector.empty
      ) {
        def apply(p: DateTime) = {
          // println(s"apply: ${p.getDayOfWeek} / ${dow.toInt}")
          // println(s"apply: ${DayOfWeekNumber.this}")
          if (dow.toInt == p.getDayOfWeek) {
            val activep = _number(p) match {
              case 1 => first
              case 2 => second
              case 3 => third
              case 4 => fourth
              case 5 => fifth
              case n => RAISE.noReachDefect(s"number = $n")
            }
          // println(s"apply activep: ${_number(p)} => ${activep}")
            if (activep)
              copy(r = r :+ p)
            else
              this
          } else {
            this
          }
            // dow match {
            //   case DateTime.Constants.MONDAY =>
            //   case DateTime.Constants.TUESDAY =>
            //   case DateTime.Constants.WEDNESDAY =>
            //   case DateTime.Constants.THURSDAY =>
            //   case DateTime.Constants.FRIDAY =>
            //   case DateTime.Constants.SATURDAY =>
            //   case DateTime.Constants.SUNDAY =>
            // }
        }

        def skip(p: DateTime) = {
//          println(s"skip: ${p.getDayOfWeek} / ${dow.toInt}")
          this
        }

        private def _number(p: DateTime): Int = (p.getDayOfMonth / 7) + 1
      }

      @annotation.tailrec
      def go(p: DateTime, z: Z): Z = {
//        println(s"go: $p")
        def next = p.plusDays(1)
        if (is_out_of_scope(now, p)) {
          z
        } else {
          if (is_in_scope(now, p))
            go(next, z.apply(p))
          else
            go(next, z.skip(p))
        }
      }
      go(start, Z()).r
    }
  }
  case class DayOfBiWeek(
    dow: SpecificDayOfWeekExpression,
    base: DateTime
  ) extends DayOfWeekExpression {
    val origin = {
      base.withDayOfWeek(dow.toInt)
    }

    def candidates(now: DateTime): Vector[DateTime] = {
      @annotation.tailrec
      def go(p: Stream[DateTime], z: Vector[DateTime]): Vector[DateTime] = p.headOption match {
        case Some(s) =>
//          println(s"DayOfWeek#candidates#go $s")
          if (is_out_of_scope(now, s))
            z
          else if (is_in_scope(now, s))
            go(p.tail, z :+ s)
          else
            go(p.tail, z)
        case None => z
      }
      go(_candidates(origin), Vector.empty)
    }

    private def _candidates(p: DateTime): Stream[DateTime] = Stream.cons(p, _candidates(p.plusDays(14)))
  }

  case class MinuteGuard(
    values: List[IntExpression] // or, sorted
  ) {
    def isActive = values.nonEmpty

    def candidates: List[Int] = {
      val a = values.flatMap(_.candidates)
      if (a.nonEmpty)
        a
      else
        List(0)
    }
  }

  case class HourGuard(
    values: List[IntExpression] // or
  ) {
    def isActive = values.nonEmpty

    def candidates: List[Int] = {
      val a = values.flatMap(_.candidates)
      if (a.nonEmpty)
        a
      else
        List(7)
    }
  }

  case class DayGuard(
    values: List[IntExpression] // or
  ) {
    def isActive = values.nonEmpty

    def candidates: List[Int] = {
      val a = values.flatMap(_.candidates)
      if (a.nonEmpty)
        a
      else
        List(1)
    }
  }

  case class MonthGuard(
    values: List[IntExpression] // or
  ) {
    def isActive = values.nonEmpty

    def candidates: List[Int] = {
      values.flatMap(_.candidates)
    }
  }

  case class DayOfWeekGuard(
    values: List[DayOfWeekExpression] // or
  ) {
    def isActive = values.nonEmpty

    def specificCandidates: List[Int] = values.collect {
      case m: SpecificDayOfWeekExpression => m.toInt
    }

    def biweeks: List[DayOfBiWeek] = values.collect {
      case m: DayOfBiWeek => m
    }

    def weeknumbers: List[DayOfWeekNumber] = values collect {
      case m: DayOfWeekNumber => m
    }
  }

  case class ActionPrepareAt(actionAt: Timestamp, prepareAt: Timestamp)

  def parse(ctx: DateTimeContext, p: String): ParseResult[CronTriggerGuard] =
    parse(ctx.dateTimeZone, p)

  def parse(tz: DateTimeZone, p: String): ParseResult[CronTriggerGuard] = {
    val values = Strings.totokens(p, " ")
    parse(tz, values)
  }

  def parse(tz: DateTimeZone, values: List[String]): ParseResult[CronTriggerGuard] = ParseResult {
    val minute = values.lift(0)
    val hour = values.lift(1)
    val day = values.lift(2)
    val month = values.lift(3)
    val dow = values.lift(4)
    val mr = _to_minute(minute)
    val hr = _to_hour(hour)
    val dr = _to_day(day)
    val ymr = _to_month(month)
    val dowr = _to_dow(dow)
    val a = _to_accurancy(mr, hr, dr, ymr, dowr)
    CronTriggerGuard(mr, hr, dr, ymr, dowr, a, tz)
  }


  private def _to_minute(v: Option[String]) = MinuteGuard(_to_ints(v))
  private def _to_hour(v: Option[String]) = HourGuard(_to_ints(v))
  private def _to_day(v: Option[String]) = DayGuard(_to_ints(v))
  private def _to_month(v: Option[String]) = MonthGuard(_to_months(v))
  private def _to_dow(v: Option[String]) = DayOfWeekGuard(_to_dows(v))

  private def _to_ints(v: Option[String]): List[IntExpression] = {
    v.cata(_to_ints, Nil)
  }

  private def _to_ints(v: String): List[IntExpression] = {
    Strings.totokens(v) flatMap { x =>
      _to_int_range(x) orElse _to_int(x)
    }
  }

  private def _to_int_range(v: String): Option[IntExpression] = {
    Strings.totokens(v, "-").toList match {
      case Nil => None
      case x :: Nil => None
      case x :: y :: xs => Some(IntRangeExpression(x.toInt, y.toInt))
    }
  }

  private def _to_int(v: String) = {
    v match {
      case "*" => None
      case _ => Some(IntValueExpression(v.toInt))
    }
  }

  private def _to_months(v: Option[String]): List[IntExpression] = {
    v.cata(_to_months, Nil)
  }

  private def _to_months(v: String): List[IntExpression] = {
    Strings.totokens(v) flatMap { x =>
      Strings.totokens(v, "-").toList match {
        case Nil => Nil
        case x :: Nil => _get_month(x).toList
        case x :: y :: xs => List(IntRangeExpression(_to_month_int(x), _to_month_int(y)))
      }
    }
  }

  private def _get_month(v: String): Option[IntExpression] = {
    v match {
      case "*" => None
      case _ => Some(_to_month(v))
    }
  }

  private def _to_month(v: String): IntValueExpression =
    IntValueExpression(_to_month_int(v))

  private def _to_month_int(v: String): Int = {
    val a = v.toLowerCase
    if (a.startsWith("jan"))
      1
    else if (a.startsWith("feb"))
      2
    else if (a.startsWith("mar"))
      3
    else if (a.startsWith("apr"))
      4
    else if (a.startsWith("may"))
      5
    else if (a.startsWith("jun"))
      6
    else if (a.startsWith("jul"))
      7
    else if (a.startsWith("Aug"))
      8
    else if (a.startsWith("sep"))
      9
    else if (a.startsWith("oct"))
      10
    else if (a.startsWith("nov"))
      11
    else if (a.startsWith("dec"))
      12
    else {
      val a = v.toInt
      require (a >= 1 & a <= 12, "Month should be number 1 to 12")
      a
    }
  }

  private def _to_dows(v: Option[String]): List[DayOfWeekExpression] = {
    val r = v.cata(_to_dows, Nil)
    r
  }

  private def _to_dows(v: String): List[DayOfWeekExpression] = {
    _to_expressions(v) flatMap { x =>
      if (x.contains('B'))
        List(_to_dow_biweek(x))
      else
        Strings.totokens(x, "-").toList match {
          case Nil => Nil
          case x :: Nil => _get_dow(x).toList
          case x :: y :: xs => _to_dow_range(_to_dow_int(x), _to_dow_int(y))
        }
    }
  }

  private def _to_expressions(p: String): List[String] = {
    sealed trait State {
      def tokens: Vector[String]
      def buffer: Vector[Char]

      def r: List[String] =
        if (buffer.isEmpty)
          tokens.toList
        else
          (tokens :+ buffer.mkString).toList
      def +(p: Char): State
    }
    case class Normal(
      tokens: Vector[String] = Vector.empty,
      buffer: Vector[Char] = Vector.empty
    ) extends State {
      def +(p: Char): State = p match {
        case '[' => Range(tokens, buffer :+ p)
        case ',' => copy(tokens = tokens :+ buffer.mkString)
        case _ => copy(buffer = buffer :+ p)
      }
    }
    case class Range(
      tokens: Vector[String] = Vector.empty,
      buffer: Vector[Char] = Vector.empty
    ) extends State {
      def +(p: Char): State =
        if (p == ']')
          Normal(tokens, buffer :+ p)
        else
          copy(buffer = buffer :+ p)
    }
    p.toVector./:(Normal(): State)(_+_).r
  }

  private def _get_dow(v: String): Option[DayOfWeekExpression] = {
    v match {
      case "*" => None
      case _ => Some(_to_dow(v))
    }
  }

  private def _to_dow(v: String): DayOfWeekExpression = try {
    if (v.contains('B'))
      _to_dow_biweek(v)
    else if (v.contains('['))
      _to_dow_weeknumber(v)
    else
      _to_dow_simple(v)
  } catch {
    case NonFatal(e) => SyntaxErrorDayOfWeekExpression(v)
  }

  private def _to_dow_simple(v: String): SpecificDayOfWeekExpression =
    _to_dow(_to_dow_int(v))

  private def _to_dow(i: Int): SpecificDayOfWeekExpression = {
    i match {
      case 0 => Sunday
      case 1 => Monday
      case 2 => Tuesday
      case 3 => Wednesday
      case 4 => Thursday
      case 5 => Friday
      case 6 => Saturday
      case 7 => Sunday
    }
  }

  private def _to_dow_range(start: Int, end: Int) = {
    (start to end).toList.map(_to_dow)
  }

  private def _to_dow_int(v: String): Int = {
    val a = v.toLowerCase
    if (a.startsWith("sun"))
      7 // for DateTime#withDayOfWeek
    else if (a.startsWith("mon"))
      1
    else if (a.startsWith("tue"))
      2
    else if (a.startsWith("wed"))
      3
    else if (a.startsWith("thu"))
      4
    else if (a.startsWith("fri"))
      5
    else if (a.startsWith("sat"))
      6
    else {
      val a = v.toInt
      require (a >= 0 & a <= 7, "Day of week should be number 0 to 7")
      if (a == 0) 7
      else a
    }
  }

  private val _biweek_regex = """(.*)B([\d-]*)""".r

  private def _to_dow_biweek(v: String): DayOfWeekExpression =
    _biweek_regex.findFirstMatchIn(v).
      map { x =>
        val dow = x.group(1)
        val base = x.group(2)
        val r = DayOfBiWeek(_to_dow_simple(dow), DateTime.parse(base))
        r
      }.getOrElse(SyntaxErrorDayOfWeekExpression(v))

  private val _weeknumber_regex = """(.*)\[([\d,]*)\]""".r

  private def _to_dow_weeknumber(v: String): DayOfWeekExpression =
    _weeknumber_regex.findFirstMatchIn(v).
      map { x =>
        val dow = x.group(1)
        val range = x.group(2)
        val numbers = Strings.totokens(range, ",").map(_.toInt).toSet
        DayOfWeekNumber(
          _to_dow_simple(dow),
          numbers.contains(1),
          numbers.contains(2),
          numbers.contains(3),
          numbers.contains(4),
          numbers.contains(5)
        )
      }.getOrElse(SyntaxErrorDayOfWeekExpression(v))

  def defaultMinuteAccuracy: Duration = 30.seconds.toStandardDuration
  def defaultHourAccuracy: Duration = 30.minutes.toStandardDuration

  def defaultDayMinuteAccuracy: Duration = 1.hour.toStandardDuration
  def defaultDayHourAccuracy: Duration = 1.hour.toStandardDuration // 2

  def defaultMonthMinuteAccuracy: Duration = 1.hours.toStandardDuration
  def defaultMonthHourAccuracy: Duration = 1.hours.toStandardDuration // 6
  def defaultMonthDayAccuracy: Duration = 1.hours.toStandardDuration // 24

  def defaultDowMinuteAccuracy: Duration = 1.hours.toStandardDuration // 2
  def defaultDowHourAccuracy: Duration = 1.hours.toStandardDuration // 6
  def defaultDowDayAccuracy: Duration = 1.hours.toStandardDuration // 24

  def defaultAccuracy: Duration = 1.hours.toStandardDuration // 24

  protected def minute_accuracy: Duration = defaultMinuteAccuracy
  protected def hour_accuracy: Duration = defaultHourAccuracy

  protected def day_minute_accuracy: Duration = defaultDayMinuteAccuracy
  protected def day_hour_accuracy: Duration = defaultDayHourAccuracy

  protected def month_minute_accuracy: Duration = defaultMonthMinuteAccuracy
  protected def month_hour_accuracy: Duration = defaultMonthHourAccuracy
  protected def month_day_accuracy: Duration = defaultMonthDayAccuracy

  protected def dow_minute_accuracy: Duration = defaultDowMinuteAccuracy
  protected def dow_hour_accuracy: Duration = defaultDowHourAccuracy
  protected def dow_day_accuracy: Duration = defaultDowDayAccuracy

  protected def default_accuracy: Duration = defaultAccuracy

  private def _to_accurancy(
    mr: MinuteGuard,
    hr: HourGuard,
    dr: DayGuard,
    ymr: MonthGuard,
    dowr: DayOfWeekGuard
  ): Duration = {
    (mr.isActive, hr.isActive, dr.isActive, ymr.isActive, dowr.isActive) match {
      case (true, false, false, false, false) => minute_accuracy
      case (true, true, false, false, false) => hour_accuracy
      case (true, true, true, false, false) => day_minute_accuracy
      case (false, true, true, false, false) => day_hour_accuracy
      case (true, _, _, true, false) => month_minute_accuracy
      case (false, true, _, true, false) => month_hour_accuracy
      case (false, false, true, true, false) => month_day_accuracy
      case (true, _, _, _, true) => dow_minute_accuracy
      case (false, true, _, _, true) => dow_hour_accuracy
      case (false, false, true, _, true) => dow_day_accuracy
      case _ => default_accuracy
    }
  }
}
