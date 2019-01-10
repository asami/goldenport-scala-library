package org.goldenport.values

import org.joda.time.DateTimeConstants
import com.github.nscala_time.time.Imports._
import Week._

/*
 * @since   Jun. 17, 2015
 * @version Jan.  9, 2019
 * @author  ASAMI, Tomoharu
 */
case class Week(
  year: Int,
  number: Int,
  baseDayOfWeek: DayOfWeek = Sunday, // Japan standard
  weekswitch: WeekSwitchStrategy = MiddleDaySwitch
) {
  def start: LocalDate = toLocalDate(year, number, baseDayOfWeek, weekswitch)
  def end: LocalDate = start.plusWeeks(1).minusDays(1)

  def +(value: Int): Week = {
    if (number > 51)
      fromLocalDate(start.plusWeeks(1), baseDayOfWeek, weekswitch)
    else
      copy(number = number + 1)
  }

  def -(value: Int): Week = {
    if (number == 1)
      fromLocalDate(start.minusWeeks(1), baseDayOfWeek, weekswitch)
    else
      copy(number = number - 1)
  }

  def <=(rhs: Week): Boolean = {
    start <= rhs.start
  }

  def <(rhs: Week): Boolean = {
    start < rhs.start
  }
}

object Week {
  sealed trait DayOfWeek {
    def value: Int // ISO 8601
  }

  case object Sunday extends DayOfWeek {
    def value = 7 // or 0
  }

  case object Monday extends DayOfWeek {
    def value = 1
  }

  case object Tuesday extends DayOfWeek {
    def value = 2
  }

  case object Wednesday extends DayOfWeek {
    def value = 3
  }

  case object Thursday extends DayOfWeek {
    def value = 4
  }

  case object Friday extends DayOfWeek {
    def value = 5
  }

  case object Saturday extends DayOfWeek {
    def value = 6
  }

  object DayOfWeek {
    val elements = Vector(
      Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday
    )

    def apply(v: Int): DayOfWeek = {
      elements find (_.value == v) getOrElse {
        throw new IllegalArgumentException(s"Unknown day of weak value: $v")
      }
    }
  }

  sealed trait WeekSwitchStrategy
  case object MiddleDaySwitch extends WeekSwitchStrategy // 1-53 monday -> thursday MySQL mode 3, ISO 8601 / sunday -> wednesday MYSQL mode 6
  case object BaseDaySwitch extends WeekSwitchStrategy // 1-53 monday -> MySQL mode 7 / sunday MySQL mode 2

  sealed trait YearSwitchStrategy
  case object ExcludeYearSwitch extends YearSwitchStrategy
  case object IncludeYearSwitch extends YearSwitchStrategy

  def fromDateTime(
    dt: DateTime,
    basedayofweek: DayOfWeek = Sunday,
    weekswitch: WeekSwitchStrategy = MiddleDaySwitch
  ): Week = {
    fromLocalDate(dt.toLocalDate, basedayofweek, weekswitch)
  }

  def fromLocalDate(
    dt: LocalDate,
    basedayofweek: DayOfWeek = Sunday,
    weekswitch: WeekSwitchStrategy = MiddleDaySwitch
  ): Week = {
    weekswitch match {
      case MiddleDaySwitch => fromLocalDateWithMiddleDay(dt, basedayofweek)
      case BaseDaySwitch => fromLocalDateWithBaseDayOfWeek(dt, basedayofweek)
    }
  }

  def fromLocalDateWithMiddleDay( // In case of Monday, ISO 8601 standard
    dt: LocalDate,
    basedayofweek: DayOfWeek = Sunday
  ): Week = {
    val year = dt.getYear
    if (_is_in_year(year, basedayofweek))
      _from_localdate_in(dt, basedayofweek)
    else
      _from_localdate_out(dt, basedayofweek)
  }

  private def _is_in_year(year: Int, dow: DayOfWeek) = {
    dow match {
      case Monday => _is_in_year_monday(year)
      case Sunday => _is_in_year_sunday(year)
      case _ => throw new IllegalArgumentException(s"Unavailable base day of week: $dow")
    }
  }

  // ISO 8601, MySQL mode 3
  private def _is_in_year_monday(year: Int) = {
    import DateTimeConstants._
    val firstday = new LocalDate(year, 1, 1)
    firstday.getDayOfWeek match {
      case MONDAY => true
      case TUESDAY => true
      case WEDNESDAY => true
      case THURSDAY => true
      case _ => false
    }
  }

  // MySQL mode 6
  private def _is_in_year_sunday(year: Int) = {
    import DateTimeConstants._
    val firstday = new LocalDate(year, 1, 1)
    firstday.getDayOfWeek match {
      case SUNDAY => true
      case MONDAY => true
      case TUESDAY => true
      case WEDNESDAY => true
      case THURSDAY => false // check MySQL spec
      case _ => false
    }
  }

  private def _from_localdate_in(
    dt: LocalDate,
    basedayofweek: DayOfWeek
  ) = {
    _from_localdate_common(dt, basedayofweek,
      Week(_, 1, basedayofweek),
      1
    )
  }

  private def _from_localdate_out(
    dt: LocalDate,
    basedayofweek: DayOfWeek
  ) = {
    _from_localdate_common(dt, basedayofweek,
      x => {
        val year = x - 1
        val number = if (_is_in_year(year, basedayofweek)) 53 else 52
        Week(year, number, basedayofweek)
      },
      0
    )
  }

  // 2014 in in (Thu)
  // 2015 in out (Thu)
  // 2016 out just(out)
  // 2017 just(out) in
  // 2018 in in
  // 2019 in in
  // 2020 in out
  // 2021 out out
  // 2022 out just
  private def _from_localdate_common(
    dt: LocalDate,
    basedayofweek: DayOfWeek,
    year2firstweekf: Int => Week,
    complementcount: Int
  ) = {
    val year = dt.getYear
    val daynumber = dt.getDayOfYear
    _first_last_baseday(year, basedayofweek) match {
      case (firstbaseday, None, true) =>
        val weeknumber = (daynumber / 7) + 1
        Week(year, weeknumber, basedayofweek)
      case (firstbaseday, None, false) =>
        if (dt < firstbaseday) {
          year2firstweekf(year)
        } else {
          val firstbasedaynumber = firstbaseday.getDayOfYear
          val fraction = firstbasedaynumber
          val weeknumber = ((daynumber - fraction) / 7) + 1 + complementcount
          Week(year, weeknumber, basedayofweek)
        }
      case (firstbaseday, Some(lastbaseday), _) =>
        if (dt < firstbaseday) {
          year2firstweekf(year)
        } else if (dt >= lastbaseday && _is_in_year(year + 1, basedayofweek)) {
          Week(year + 1, 1, basedayofweek)
        } else {
          val firstbasedaynumber = firstbaseday.getDayOfYear
          val fraction = firstbasedaynumber
          val weeknumber = ((daynumber - fraction) / 7) + 1 + complementcount
          Week(year, weeknumber, basedayofweek)
        }
    }
  }

  def fromDateTimeWithBaseDayOfWeek(
    dt: DateTime,
    basedayofweek: DayOfWeek = Sunday
  ): Week = {
    fromLocalDateWithBaseDayOfWeek(dt.toLocalDate, basedayofweek)
  }

  def fromLocalDateWithBaseDayOfWeek(
    dt: LocalDate,
    basedayofweek: DayOfWeek = Sunday,
    yearswitch: YearSwitchStrategy = IncludeYearSwitch
  ): Week = {
    val year = dt.getYear
    val daynumber = dt.getDayOfYear
    _first_last_baseday(year, basedayofweek) match {
      case (firstbaseday, None, true) =>
        val weeknumber = (daynumber / 7) + 1
        Week(year, weeknumber, basedayofweek)
      case (firstbaseday, None, false) =>
        if (dt < firstbaseday) {
          Week(year, 1, basedayofweek)
        } else {
          val firstbasedaynumber = firstbaseday.getDayOfYear
          val fraction = firstbasedaynumber
          val weeknumber = ((daynumber - fraction) / 7) + 2
          Week(year, weeknumber, basedayofweek)
        }
      case (firstbaseday, Some(lastbaseday), _) =>
        if (dt < firstbaseday) {
          Week(year, 1, basedayofweek)
        } else if (dt >= lastbaseday) {
          Week(year + 1, 1, basedayofweek)
        } else {
          val firstbasedaynumber = firstbaseday.getDayOfYear
          val fraction = firstbasedaynumber
          val weeknumber = ((daynumber - fraction) / 7) + 2
          Week(year, weeknumber, basedayofweek)
        }
    }
  }

  private def _first_last_baseday(
    year: Int,
    basedayofweek: DayOfWeek
  ): (LocalDate, Option[LocalDate], Boolean) = {
    val firstday = new LocalDate(year, 1, 1)
    toDayOfWeek(firstday) match {
      case dow if dow == basedayofweek => (firstday, None, true)
      case dow =>
        val firstbaseday = toNext(firstday, basedayofweek)
        val nextfirstday = new LocalDate(year + 1, 1, 1)
        toDayOfWeek(nextfirstday) match {
          case dow2 if dow2 == basedayofweek => (firstbaseday, None, false)
          case dow2 =>
            val lastbaseday = toPrevious(nextfirstday, basedayofweek)
            (firstbaseday, Some(lastbaseday), false)
        }
    }
  }

  def toLocalDate(
    year: Int,
    number: Int,
    baseDayOfWeek: DayOfWeek = Sunday, // Japan standard
    weekswitch: WeekSwitchStrategy = MiddleDaySwitch
  ): LocalDate = {
    weekswitch match {
      case MiddleDaySwitch => _start_day_middleday(year, number, baseDayOfWeek)
      case BaseDaySwitch => _start_day_baseday(year, number, baseDayOfWeek)
    }
  }

  // new LocalDate().withYear(year).withWeekOfWeekyear(week)
  private def _start_day_middleday(year: Int, number: Int, dow: DayOfWeek) = {
    _first_last_baseday(year, dow) match {
      case (firstbaseday, _, true) =>
        new LocalDate().withYear(year).withDayOfYear(number * 7 - 6)
      case (firstbaseday, _, false) =>
        val firstbasedaynumber = firstbaseday.getDayOfYear
        val fraction = (8 - firstbasedaynumber)
        if (_is_in_year(year, dow))
          new LocalDate().withYear(year).withDayOfYear(number * 7 - 6).minusDays(fraction)
        else
          new LocalDate().withYear(year).withDayOfYear(number * 7 - 6).minusDays(fraction).plusWeeks(1)
    }
  }

  private def _start_day_baseday(year: Int, number: Int, dow: DayOfWeek) = {
    _first_last_baseday(year, dow) match {
      case (firstbaseday, _, true) =>
        new LocalDate().withYear(year).withDayOfYear(number * 7 - 6)
      case (firstbaseday, _, false) =>
        val firstbasedaynumber = firstbaseday.getDayOfYear
        val fraction = firstbasedaynumber - 1
        if (true) // IncludeYearSwitch
          new LocalDate().withYear(year).withDayOfYear(number * 7 - 6).plusDays(fraction).minusWeeks(1)
        else
          new LocalDate().withYear(year).withDayOfYear(number * 7 - 6 + fraction)
    }
  }

  def toNext(d: LocalDate, dow: DayOfWeek): LocalDate = {
    @annotation.tailrec
    def go(x: LocalDate): LocalDate = {
      if (x.getDayOfWeek == dow.value)
        x
      else
        go(x.plusDays(1))
    }
    go(d.plusDays(1))
  }

  def toPrevious(d: LocalDate, dow: DayOfWeek): LocalDate = {
    @annotation.tailrec
    def go(x: LocalDate): LocalDate = {
      if (x.getDayOfWeek == dow.value)
        x
      else
        go(x.minusDays(1))
    }
    go(d.minusDays(1))
  }

  def toDayOfWeek(d: LocalDate): DayOfWeek = {
    DayOfWeek(d.getDayOfWeek)
  }
}
