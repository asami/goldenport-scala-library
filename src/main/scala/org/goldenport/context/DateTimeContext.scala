package org.goldenport.context

import java.util.TimeZone
import java.sql.Timestamp
import org.joda.time._
import org.goldenport.values.LocalDateTimeInterval
import org.goldenport.util.DateTimeUtils

/*
 * See org.goldenport.util.DateTimeContext
 * 
 * @since   Jan. 22, 2021
 *  version Feb. 28, 2021
 *  version Apr. 26, 2021
 *  version Nov. 13, 2021
 *  version Feb. 16, 2022
 *  version Apr.  4, 2022
 *  version May.  2, 2022
 *  version Nov. 13, 2022
 *  version Apr.  6, 2023
 * @version Oct. 14, 2024
 * @author  ASAMI, Tomoharu
 */
case class DateTimeContext(
  base: DateTime,
  gap: Period,
  dateTimeZone: DateTimeZone
) {
  def current: DateTime = DateTime.now(dateTimeZone) plus gap

  def currentYear: Int = current.year.get
  def currentMonth: Int = current.monthOfYear.get
  def currentWeek: Int = current.weekOfWeekyear.get
  def currentDay: Int = current.dayOfMonth.get
  def currentHour: Int = current.hourOfDay.get

  def currentTimeMillis: Long = datetime.getMillis
  def unixTime: Long = currentTimeMillis / 1000
  def timestamp: Timestamp = new Timestamp(currentTimeMillis)
  def datetime: DateTime = base plus gap
  lazy val timezone: TimeZone = DateTimeUtils.dateTimeZoneToTz(dateTimeZone)

  def toDateTime(p: java.sql.Timestamp): DateTime = toDateTime(p.getTime)
  def toDateTime(p: Long): DateTime = new DateTime(p, dateTimeZone)

  def intervalBoundaryMonth: LocalDateTimeInterval = {
    val start = datetime
    val end = start.plusMonths(1)
    LocalDateTimeInterval.openUpper(start, end)
  }
  def intervalThisMonth: LocalDateTimeInterval = {
    val start = DateTimeUtils.startOfFirstDayOfThisMonth(datetime)
    val end = DateTimeUtils.startOfFirstDayOfNextMonth(datetime)
    LocalDateTimeInterval.openUpper(start, end)
  }

      // case Handoff.Payment.Kind.Boundary =>
      //   // context.dateTimeContext.intervalBoundaryMonth
      //   val datetime = context.dateTimeContext.datetime
      //   val start = datetime
      //   val end = start.plusMonths(1)
      //   LocalDateTimeInterval.openUpper(start, end)
      // case Handoff.Payment.Kind.EndOfMonth =>
      //   // context.dateTimeContext.intervalThisMonth
      //   val datetime = context.dateTimeContext.datetime
      //   val start = DateTimeUtils.startOfFirstDayOfThisMonth(datetime)
      //   val end = DateTimeUtils.startOfFirstDayOfNextMonth(datetime)
      //   LocalDateTimeInterval.openUpper(start, end)

}

object DateTimeContext {
  def apply(p: DateTime): DateTimeContext = apply(p, p.getZone)

  def apply(p: DateTime, tz: DateTimeZone): DateTimeContext = DateTimeContext(p, Period.ZERO, tz)

  def now(tz: TimeZone): DateTimeContext = now(DateTimeUtils.tzToDateTimeZone(tz))

  def now(tz: DateTimeZone): DateTimeContext = DateTimeContext(DateTime.now, Period.ZERO, tz)

  def now(): DateTimeContext = {
    val dt = DateTime.now
    DateTimeContext(dt, Period.ZERO, dt.getZone)
  }

  def nowJst(): DateTimeContext = {
    val dt = DateTime.now
    DateTimeContext(dt, Period.ZERO, DateTimeUtils.jodajst)
  }

  def nowEst(): DateTimeContext = {
    val dt = DateTime.now
    DateTimeContext(dt, Period.ZERO, DateTimeUtils.jodaEst)
  }

  def nowGmt(): DateTimeContext = {
    val dt = DateTime.now
    DateTimeContext(dt, Period.ZERO, DateTimeUtils.jodagmt)
  }

  def nowEuropeBerlin(): DateTimeContext = {
    val dt = DateTime.now
    DateTimeContext(dt, Period.ZERO, DateTimeUtils.jodaEuropeBerlin)
  }

  def nowEuropeZurich(): DateTimeContext = {
    val dt = DateTime.now
    DateTimeContext(dt, Period.ZERO, DateTimeUtils.jodaEuropeZurich)
  }


  def create(y: Int, m: Int, d: Int, h: Int, mi: Int): DateTimeContext = 
    create(DateTime.now.getZone, y, m, d, h, mi)

  def create(tz: DateTimeZone, y: Int, m: Int, d: Int, h: Int, mi: Int): DateTimeContext = {
    val base = DateTime.now(tz)
    val a = new DateTime(y, m, d, h, mi, tz)
    DateTimeContext(base, new Period(base, a), tz)
  }

  def create(tz: DateTimeZone, ts: Long): DateTimeContext = {
    val base = DateTime.now(tz)
    val a = new DateTime(ts, tz)
    DateTimeContext(base, new Period(base, a), tz)
  }

  def create(
    tz: DateTimeZone,
    basets: Long,
    appts: Long
  ): DateTimeContext = {
    val base = new DateTime(basets, tz)
    val a = new DateTime(appts, tz)
    DateTimeContext(base, new Period(base, a), tz)
  }
}
