package org.goldenport.context

import org.joda.time._
import org.goldenport.util.DateTimeUtils

/*
 * @since   Jan. 22, 2021
 * @version Feb. 28, 2021
 * @author  ASAMI, Tomoharu
 */
case class DateTimeContext(
  base: DateTime,
  gap: Period,
  dateTimeZone: DateTimeZone
) {
  def current: DateTime = DateTime.now(dateTimeZone) plus gap

  // def currentYear: Int = current.year.get
  // def currentMonth: Int = current.monthOfYear.get
  // def currentWeek: Int = current.weekOfWeekyear.get
  // def currentDay: Int = current.dayOfMonth.get
  // def currentHour: Int = current.hourOfDay.get
}

object DateTimeContext {
  def apply(p: DateTime): DateTimeContext = apply(p, p.getZone)

  def apply(p: DateTime, tz: DateTimeZone): DateTimeContext = DateTimeContext(p, Period.ZERO, tz)

  def now(tz: DateTimeZone) = DateTimeContext(DateTime.now, Period.ZERO, tz)

  def now() = {
    val dt = DateTime.now
    DateTimeContext(dt, Period.ZERO, dt.getZone)
  }
}
