package org.goldenport.util

import scala.util.Try
import scala.concurrent.duration.FiniteDuration
import java.util.{Date, Locale, TimeZone}
import java.sql.Timestamp
import java.text.SimpleDateFormat
import org.joda.time.{DateTime, LocalDate, DateTimeZone}
import org.joda.time.LocalDateTime
import org.joda.time.format.ISODateTimeFormat

/*
 * TODO unify org.goldenport.record.util
 *
 * @since   Jun. 10, 2014
 *  version Jul. 27, 2014
 *  version Aug. 27, 2015
 *  version Mar. 17, 2016
 *  version Sep.  9, 2016
 *  version Jan. 19, 2017
 *  version Aug. 29, 2017
 *  version Jan. 27, 2022
 *  version Feb. 19, 2022
 * @version Nov. 17, 2022
 * @author  ASAMI, Tomoharu
 */
object TimestampUtils {
  val millisInSecond: Long = 1000
  val millisInMinute: Long = millisInSecond * 60
  val millisInHour: Long = millisInMinute * 60
  val millisInDay: Long = millisInHour * 24

  private val _iso_datetime_parser = ISODateTimeFormat.dateTimeParser
  private val _rss_datetime_format = {
    val sdf = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss zzz", Locale.ENGLISH)
//    sdf.setTimeZone(gmt)
    sdf
  }

  def parse(tz: DateTimeZone, s: String): Timestamp = {
    parseLong(s) orElse parseIsoOption(tz, s) orElse parseRssOption(tz, s) getOrElse {
      throw new IllegalArgumentException(s"Illegal datetime '$s'")
    }
  }

  def parseIsoOption(tz: DateTimeZone, s: String): Option[Timestamp] = {
    Try(parseIso(tz, s)).toOption
  }

  def parseIso(tz: DateTimeZone, s: String): Timestamp = {
    val dt = DateTimeUtils.parseIsoDateTime(s, tz)
    val a = dt.getMillis
    new Timestamp(a)
  }

  def parseRssOption(tz: DateTimeZone, s: String): Option[Timestamp] = {
    Try(parseRss(tz, s)).toOption
  }

  def parseRss(tz: DateTimeZone, s: String): Timestamp = {
    synchronized {
      val sdf = _rss_datetime_format
      sdf.setTimeZone(DateTimeUtils.dateTimeZoneToTz(tz))
      val a = sdf.parse(s)
      new Timestamp(a.getTime)
    }
  }

  def parse(s: String): Timestamp = {
    parseLong(s) orElse parseIsoOption(s) orElse parseRssOption(s) getOrElse {
      throw new IllegalArgumentException(s"Illegal datetime '$s'")
    }
  }

  def parseLong(s: String): Option[Timestamp] = NumberUtils.getLong(s).map(new Timestamp(_))

  def parseIsoOption(s: String): Option[Timestamp] = {
    Try(parseIso(s)).toOption
  }

  def parseIso(s: String): Timestamp = {
    val a = _iso_datetime_parser.parseMillis(s)
    new Timestamp(a)
  }

  def parseRssOption(s: String): Option[Timestamp] = {
    Try(parseRss(s)).toOption
  }

  def parseRss(s: String): Timestamp = {
    synchronized {
      val a = _rss_datetime_format.parse(s)
      new Timestamp(a.getTime)
    }
  }

  def toTimestampJst(year: Int, month: Int, day: Int, hour: Int, minute: Int) =
    new Timestamp(DateTimeUtils.toDateTimeJst(year, month, day, hour, minute).getMillis)

  def toTimestampGmt(year: Int, month: Int, day: Int, hour: Int, minute: Int) =
    new Timestamp(DateTimeUtils.toDateTimeGmt(year, month, day, hour, minute).getMillis)

  def toTimestamp(p: DateTime) = new Timestamp(p.getMillis)

  //
  def toLocalDate(tz: TimeZone, base: Timestamp): LocalDate = {
    new DateTime(base.getTime, DateTimeZone.forTimeZone(tz)).toLocalDate
  }

  def toLocalDate(tz: DateTimeZone, base: Timestamp): LocalDate = {
    new DateTime(base.getTime, tz).toLocalDate
  }

  def toLocalDateTime(tz: TimeZone, base: Timestamp): LocalDateTime = {
    new DateTime(base.getTime, DateTimeZone.forTimeZone(tz)).toLocalDateTime
  }

  def toLocalDateTime(tz: DateTimeZone, base: Timestamp): LocalDateTime = {
    new DateTime(base.getTime, tz).toLocalDateTime
  }

  //
  def isInMinute(ts: Timestamp): Boolean = isInMinute(ts.getTime)
  def isInMinuteWithSkew(ts: Timestamp, skew: Long): Boolean =
    isInMinute(ts.getTime - skew)
  def isInMinute(target: Long): Boolean =
    isInMinute(System.currentTimeMillis, target)
  def isInMinute(current: Long, target: Long): Boolean =
    current <= target + millisInMinute
  def isInSecond(ts: Timestamp): Boolean = isInSecond(ts.getTime)
  def isInSecondWithSkew(ts: Timestamp, skew: Long): Boolean =
    isInSecond(ts.getTime - skew)
  def isInSecond(target: Long): Boolean =
    isInSecond(System.currentTimeMillis, target)
  def isInSecond(current: Long, target: Long): Boolean =
    current <= target + millisInSecond

  def isIn(ts: Timestamp, duration: FiniteDuration): Boolean =
    isIn(ts, duration.toMillis)

  def isIn(ts: Timestamp, duration: Long): Boolean =
    isIn(ts.getTime, duration)

  def isIn(ts: Long, duration: Long): Boolean =
    isIn(System.currentTimeMillis, ts, duration)

  def isIn(current: Long, target: Long, duration: Long): Boolean =
    current <= target + duration

  def isInHours(ts: Timestamp, hours: Int): Boolean = isInHours(System.currentTimeMillis, ts.getTime, hours)

  def isInHours(current: Long, ts: Timestamp, hours: Int): Boolean = isInHours(current, ts.getTime, hours)

  def isInHours(ts: Long, hours: Int): Boolean = isInHours(System.currentTimeMillis, ts, hours)

  def isInHours(current: Long, ts: Long, hours: Int): Boolean = 
    isIn(current, ts, hours * millisInHour)

  def isInMinutes(ts: Timestamp, minutes: Int): Boolean = isInMinutes(System.currentTimeMillis, ts.getTime, minutes)

  def isInMinutes(current: Long, ts: Timestamp, minutes: Int): Boolean = isInMinutes(current, ts.getTime, minutes)

  def isInMinutes(ts: Long, minutes: Int): Boolean = isInMinutes(System.currentTimeMillis, ts, minutes)

  def isInMinutes(current: Long, ts: Long, minutes: Int): Boolean = 
    isIn(current, ts, minutes * millisInMinute)

  def isInSeconds(ts: Timestamp, seconds: Int): Boolean = isInSeconds(System.currentTimeMillis, ts.getTime, seconds)

  def isInSeconds(current: Long, ts: Timestamp, seconds: Int): Boolean = isInSeconds(current, ts.getTime, seconds)

  def isInSeconds(ts: Long, seconds: Int): Boolean = isInSeconds(System.currentTimeMillis, ts, seconds)

  def isInSeconds(current: Long, ts: Long, seconds: Int): Boolean = 
    isIn(current, ts, seconds * millisInSecond)

  def startOfFirstDayOfNextMonth(p: Timestamp, tz: TimeZone): Timestamp =
    toTimestamp(DateTimeUtils.startOfFirstDayOfNextMonth(DateTimeUtils.toDateTime(p, tz)))

  def startOfFirstDayOfThisMonth(p: Timestamp, tz: TimeZone): Timestamp =
    toTimestamp(DateTimeUtils.startOfFirstDayOfThisMonth(DateTimeUtils.toDateTime(p, tz)))

  def endOfLastDayOfThisMonth(p: Timestamp, tz: TimeZone): Timestamp =
    toTimestamp(DateTimeUtils.endOfLastDayOfThisMonth(DateTimeUtils.toDateTime(p, tz)))

  def countOfMonthsAlmost(start: Timestamp, end: Timestamp, tz: TimeZone): Int =
    LocalDateTimeUtils.countOfMonthsAlmost(toLocalDateTime(tz, start), toLocalDateTime(tz, end))

  def countOfMonthsPassed(start: Timestamp, end: Timestamp, tz: TimeZone): Int =
    LocalDateTimeUtils.countOfMonthsPassed(toLocalDateTime(tz, start), toLocalDateTime(tz, end))

  def countOfYearsAlmost(start: Timestamp, end: Timestamp, tz: TimeZone): Int =
    LocalDateTimeUtils.countOfYearsAlmost(toLocalDateTime(tz, start), toLocalDateTime(tz, end))

  def countOfYearsPassed(start: Timestamp, end: Timestamp, tz: TimeZone): Int =
    LocalDateTimeUtils.countOfYearsPassed(toLocalDateTime(tz, start), toLocalDateTime(tz, end))
}
