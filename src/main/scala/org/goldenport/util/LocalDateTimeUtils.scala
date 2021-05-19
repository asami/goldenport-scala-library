package org.goldenport.util

import org.joda.time._
import org.joda.time.DateTimeConstants._

/*
 * @since   Jan.  2, 2019
 * @version Jan. 23, 2021
 * @author  ASAMI, Tomoharu
 */
object LocalDateTimeUtils {
  def parse(s: String): LocalDateTime = LocalDateTime.parse(s)

  def toDisplayString(p: LocalDateTime): String = p.toString

  def toBasicDateString(p: LocalDateTime): String = p.toString

  def dayCount(start: LocalDateTime, end: LocalDateTime): Int = {
    val a = end.toDateTime.withTimeAtStartOfDay.getMillis - start.toDateTime.withTimeAtStartOfDay.getMillis + 1
    _count_day(a)
  }

  def dayLength(start: LocalDateTime, end: LocalDateTime): Int = {
    val a = end.toDateTime.getMillis - start.toDateTime.getMillis + 1
    _count_day(a)
  }

  private def _count_day(x: Long): Int = {
    val round = if (x % MILLIS_PER_MINUTE == 0) 0 else 1
    ((x / MILLIS_PER_DAY) + round).toInt
  }
}

