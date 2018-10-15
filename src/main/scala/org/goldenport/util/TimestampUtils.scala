package org.goldenport.util

import scala.util.Try
import scala.concurrent.duration.FiniteDuration
import java.util.{Date, Locale, TimeZone}
import java.sql.Timestamp
import java.text.SimpleDateFormat
import org.joda.time.{DateTime, LocalDate, DateTimeZone}
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
 * @version Aug. 29, 2017
 * @author  ASAMI, Tomoharu
 */
object TimestampUtils {
  val millisInSecond: Long = 1000
  val millisInMinute: Long = millisInSecond * 60
  val millisInHour: Long = millisInMinute * 60
  val millisInDay: Long = millisInHour * 24

  private val _iso_datetime_parser = ISODateTimeFormat.dateTimeParser
  private val _rss_datatime_format = {
    val sdf = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss zzz", Locale.ENGLISH)
//    sdf.setTimeZone(gmt)
    sdf
  }

  def parse(s: String): Timestamp = {
    parseIsoOption(s) orElse parseRssOption(s) getOrElse {
      throw new IllegalArgumentException(s"Illegal datetime '$s'")
    }
  }

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
      val a = _rss_datatime_format.parse(s)
      new Timestamp(a.getTime)
    }
  }

  def toTimestampJst(year: Int, month: Int, day: Int, hour: Int, minute: Int) =
    new Timestamp(DateTimeUtils.toDateTimeJst(year, month, day, hour, minute).getMillis)

  def toTimestampGmt(year: Int, month: Int, day: Int, hour: Int, minute: Int) =
    new Timestamp(DateTimeUtils.toDateTimeGmt(year, month, day, hour, minute).getMillis)

  //
  def toLocalDate(tz: TimeZone, base: Timestamp): LocalDate = {
    new DateTime(base.getTime, DateTimeZone.forTimeZone(tz)).toLocalDate
  }

  def toLocalDate(tz: DateTimeZone, base: Timestamp): LocalDate = {
    new DateTime(base.getTime, tz).toLocalDate
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
}
