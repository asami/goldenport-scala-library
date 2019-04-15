package org.goldenport.values

import org.joda.time._

/*
 * @since   Aug. 26, 2015
 * @version Jan.  9, 2019
 * @author  ASAMI, Tomoharu
 */
case class RichDateTimeZone(underlying: DateTimeZone) extends AnyVal {
  def yearFirst(year: Int): DateTime = {
    new DateTime(year, 1, 1, 0, 0, underlying)
  }

  def yearLast(year: Int): DateTime = {
    val dt = new DateTime(year, 1, 1, 0, 0, underlying)
    dt.plusYears(1).minusMillis(1)
  }

  def monthFirst(year: Int, month: Int): DateTime = {
    new DateTime(year, month, 1, 0, 0, underlying)
  }

  def monthLast(year: Int, month: Int): DateTime = {
    val dt = new DateTime(year, month, 1, 0, 0, underlying)
    dt.plusMonths(1).minusMillis(1)
  }

  def weekFirst(year: Int, week: Int): DateTime = {
    ???
  }

  def weekLast(year: Int, week: Int): DateTime = {
    // val dt = new DateTime(year, month, 1, 0, 0, timezoneJoda)
    // dt.plusMonths(1).minusMillis(1)
    ???
  }

  def dayFirst(year: Int, month: Int, day: Int): DateTime = {
    new DateTime(year, month, day, 0, 0, underlying)
  }

  def dayLast(year: Int, month: Int, day: Int): DateTime = {
    val dt = new DateTime(year, month, day, 0, 0, underlying)
    dt.plusDays(1).minusMillis(1)
  }
}
