package org.goldenport.util

import com.github.nscala_time.time.Imports._
import org.joda.time.DateTimeConstants._
import org.goldenport.context.Consequence

/*
 * @since   Jan.  2, 2019
 *  version Jan. 23, 2021
 *  version Jan. 27, 2022
 * @version Dec. 12, 2022
 * @author  ASAMI, Tomoharu
 */
object LocalDateTimeUtils {
  def parse(s: String): LocalDateTime = LocalDateTime.parse(s)

  def consequenceLocalDateTime(p: String): Consequence[LocalDateTime] =
    Consequence(parse(p))

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

  def countOfMonthsAlmost(start: LocalDateTime, end: LocalDateTime): Int = {
    @annotation.tailrec
    def _go_(p: LocalDateTime, count: Int): Int = {
      // println(s"$p - $end: $count")
      if (p == end)
        count + 1
      else if (p > end)
        count
      else
        _go_(p.plusMonths(1), count + 1)
    }
    _go_(start, 0)
  }

  def countOfMonthsPassed(start: LocalDateTime, end: LocalDateTime): Int = {
    @annotation.tailrec
    def _go_(p: LocalDateTime, count: Int): Int = {
      // println(s"$p - $end: $count")
      if (p == end)
        count + 1
      else if (p >= end)
        count
      else
        _go_(p.plusMonths(1), count + 1)
    }
    _go_(start, -1)
  }

  def countOfYearsAlmost(start: LocalDateTime, end: LocalDateTime): Int = {
    @annotation.tailrec
    def _go_(p: LocalDateTime, count: Int): Int = {
      if (p > end)
        count
      else
        _go_(p.plusYears(1), count)
    }
    _go_(start, 1)
  }

  def countOfYearsPassed(start: LocalDateTime, end: LocalDateTime): Int = {
    @annotation.tailrec
    def _go_(p: LocalDateTime, count: Int): Int = {
      if (p >= end)
        count
      else
        _go_(p.plusYears(1), count + 1)
    }
    _go_(start, 0)
  }
}

