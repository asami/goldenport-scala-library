package org.goldenport.util

import scala.util.Try
import java.util.{Date, TimeZone, Locale}
import java.sql.Timestamp
import java.text.SimpleDateFormat
import org.joda.time._
import org.joda.time.format.{ISODateTimeFormat, DateTimeFormat}

/*
 * TODO unify org.goldenport.record.util.DateTimeUtils
 *
 * @since   Jul. 25, 2014
 *  version Aug. 26, 2015
 *  version Oct. 21, 2015
 *  version Feb.  3, 2016
 *  version Sep.  4, 2016
 *  version Jan. 20, 2017
 *  version Aug. 29, 2017
 *  version Oct.  3, 2017
 * @version Dec. 29, 2018
 * @author  ASAMI, Tomoharu
 */
object DateTimeUtils {
  val gmt = TimeZone.getTimeZone("GMT")
  val jst = TimeZone.getTimeZone("JST")
  val jodagmt = DateTimeZone.forID("GMT")
  val jodajst = DateTimeZone.forID("Asia/Tokyo")
  val isoFormatter = ISODateTimeFormat.dateTimeNoMillis()
  val isoUtcFormatter = isoFormatter.withZoneUTC
  val isoJstFormatter = isoFormatter.withZone(jodajst)
  val isoJstParser = ISODateTimeFormat.dateTimeParser.withZone(jodajst)
  val basicFormattter = ISODateTimeFormat.basicDate.withZoneUTC // yyyyMMdd'T'HHmmss.SSSZ
  val basicFormattterJst = basicFormattter.withZone(jodajst)
  val simpleFormatter = DateTimeFormat.forPattern("yyyyMMdd HHmmss").withZoneUTC
  val simpleFormatterJst = simpleFormatter.withZone(jodajst)
  val httpDateTimeFormat = {
    // http://candrews.integralblue.com/2009/02/http-caching-header-aware-servlet-filter/
    val sdf = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss z", Locale.US)
    sdf.setTimeZone(gmt)
    sdf
  }

  def toIsoDateTimeString(dt: DateTime, tz: DateTimeZone): String = {
    toIsoDateTimeString(dt.getMillis, tz)
  }

  def toIsoDateTimeString(dt: java.sql.Timestamp, tz: DateTimeZone): String = {
    toIsoDateTimeString(dt.getTime, tz)
  }

  def toIsoDateTimeString(dt: Long, tz: DateTimeZone): String = {
    val fmt = if (tz == jodajst)
      isoJstFormatter
    else if (tz == jodagmt)
      isoUtcFormatter
    else
      isoFormatter.withZone(tz)
    fmt.print(dt)
  }

  def toIsoDateTimeStringJst(dt: DateTime): String = {
    toIsoDateTimeString(dt, jodajst)
  }

  def toIsoDateTimeStringJst(dt: java.sql.Timestamp): String = {
    toIsoDateTimeString(dt, jodajst)
  }

  def toIsoDateTimeStringJst(dt: Long): String = {
    toIsoDateTimeString(dt, jodajst)
  }

  def toBasicDateStringJst(dt: DateTime): String = {
    basicFormattterJst.print(dt)
  }

  def toWebStringJst(ts: java.sql.Timestamp): String = {
    toWebStringJst(new DateTime(ts.getTime))
  }

  def toWebStringJst(dt: DateTime): String = {
    val jst = TimeZone.getTimeZone("JST")
    val df = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss")
    df.setTimeZone(jst)
    df.format(dt.getMillis)
  }

  def toNaturalStringJst(ts: java.sql.Timestamp): String = {
    toNaturalStringJst(new DateTime(ts.getTime))
  }

  def toNaturalStringJst(dt: DateTime) = {
    val jst = TimeZone.getTimeZone("JST")
    val df = new SimpleDateFormat("yyyy年M月d日H時m分")
    df.setTimeZone(jst)
    df.format(dt.getMillis)
  }

  def toNaturalStringJst(
    start: Option[DateTime],
    end: Option[DateTime],
    inclusive: Boolean
  ): String = {
    getNaturalStringJst(start, end, inclusive) getOrElse "全期間"
  }

  def getNaturalStringJst(
    start: Option[DateTime],
    end: Option[DateTime],
    inclusive: Boolean
  ): Option[String] = {
    if (inclusive)
      getNaturalStringJstInclusive(start, end)
    else
      getNaturalStringJstExclusive(start, end)
  }


  def getNaturalStringJstInclusive(
    start: Option[DateTime],
    end: Option[DateTime]
  ): Option[String] = {
    def dt(x: DateTime) = toNaturalStringJst(x)
    (start, end) match {
      case (Some(s), Some(e)) => Some(s"${dt(s)} 〜 ${dt(e)}")
      case (Some(s), None) => Some(s"${dt(s)} 〜")
      case (None, Some(e)) => Some(s"〜 ${dt(e)}")
      case (None, None) => None
    }
  }

  def getNaturalStringJstExclusive(
    start: Option[DateTime],
    end: Option[DateTime]
  ): Option[String] = {
    def dt(x: DateTime) = toNaturalStringJst(x)
    (start, end) match {
      case (Some(s), Some(e)) => Some(s"${dt(s)} 〜 ${dt(e)}")
      case (Some(s), None) => Some(s"{dt(s)} 〜")
      case (None, Some(e)) => Some(s"〜 ${dt(e)}")
      case (None, None) => None
    }
  }

  def toSimpleStringGmt(dt: DateTime): String = simpleFormatter.print(dt)

  def toSimpleStringGmt(ts: java.sql.Timestamp): String = simpleFormatter.print(ts.getTime)

  def toSimpleStringJst(dt: DateTime): String = simpleFormatterJst.print(dt)

  def toSimpleStringJst(ts: java.sql.Timestamp): String = simpleFormatterJst.print(ts.getTime)

  def toSimpleString24Jst(dt: java.sql.Timestamp): String = ???

  // Parser
  def parseIsoDateTime(s: String, tz: DateTimeZone): DateTime = Try(
    if (tz == jodajst)
      isoJstFormatter.parseDateTime(s)
    else
      ISODateTimeFormat.dateTimeParser.withZone(tz).parseDateTime(s)
  ).recover {
    case e => LocalDateTime.parse(s).toDateTime(tz)
  }.recover {
    case e => LocalDate.parse(s).toDateTimeAtStartOfDay(tz)
  }.get

  def parseIsoDateTimeJst(s: String): DateTime = {
    parseIsoDateTime(s, jodajst)
  }

  // Convert
  def toDateTime(dt: java.util.Date, tz: DateTimeZone): DateTime =
    toDateTime(dt.getTime, tz)

  def toDateTime(dt: java.sql.Timestamp, tz: DateTimeZone): DateTime = {
    toDateTime(dt.getTime, tz)
  }

  def toDateTime(dt: Long, tz: DateTimeZone): DateTime = {
    new DateTime(dt, tz)
  }

  def toDateTime(dt: DateTime, tz: DateTimeZone): DateTime = {
    toDateTime(dt.getMillis, tz)
  }

  def toDateTimeGmt(dt: java.util.Date): DateTime =
    toDateTime(dt, jodagmt)

  def toDateTimeGmt(dt: java.sql.Timestamp): DateTime =
    toDateTime(dt, jodagmt)

  def toDateTimeGmt(dt: Long): DateTime =
    toDateTime(dt, jodagmt)

  def toDateTimeGmt(dt: DateTime): DateTime = toDateTimeGmt(dt.getMillis)

  def toDateTimeGmt(year: Int, month: Int, day: Int, hour: Int, minute: Int): DateTime =
    new DateTime(year, month, day, hour, minute, jodagmt)

  def toDateTimeJst(dt: java.util.Date): DateTime = {
    toDateTime(dt, jodajst)
  }

  def toDateTimeJst(dt: java.sql.Timestamp): DateTime = {
    toDateTime(dt, jodajst)
  }

  def toDateTimeJst(dt: Long): DateTime = {
    toDateTime(dt, jodajst)
  }

  def toDateTimeJst(dt: DateTime): DateTime = toDateTimeJst(dt.getMillis)

  def toDateTimeJst(year: Int, month: Int, day: Int): DateTime =
    new DateTime(year, month, day, 0, 0, jodajst)

  def toDateTimeJst(year: Int, month: Int, day: Int, hour: Int, minute: Int): DateTime =
    new DateTime(year, month, day, hour, minute, jodajst)

  def toDateTimeGmtFromJst(year: Int, month: Int, day: Int, hour: Int, minute: Int): DateTime =
    toDateTimeGmt(toDateTimeJst(year, month, day, hour, minute).getMillis)

  // Year-Month-Day
  def toYMDGmt(dt: java.util.Date): (Int, Int, Int) =
    toYMDGmt(dt.getTime)

  def toYMDGmt(dt: java.sql.Timestamp): (Int, Int, Int) =
    toYMDGmt(dt.getTime)

  def toYMDGmt(dt: Long): (Int, Int, Int) = {
    val a = toDateTimeGmt(dt)
    (a.getYear, a.getMonthOfYear, a.getDayOfMonth)
  }

  def toYMDJst(dt: java.util.Date): (Int, Int, Int) =
    toYMDJst(dt.getTime)

  def toYMDJst(dt: java.sql.Timestamp): (Int, Int, Int) =
    toYMDJst(dt.getTime)

  def toYMDJst(dt: Long): (Int, Int, Int) = {
    val a = toDateTimeJst(dt)
    (a.getYear, a.getMonthOfYear, a.getDayOfMonth)
  }

  /*
   * Functions
   */
  def asPreviousMonth(dt: DateTime): Int = dt.minusMonths(1).getMonthOfYear
  def asPreviousDay(dt: DateTime): Int = dt.minusDays(1).getDayOfMonth

  /*
   * HTTP
   */
  def httpDateTimeString(ts: Long): String = {
    httpDateTimeString(new Date(ts))
  }

  def httpDateTimeString(date: Date): String = {
    httpDateTimeFormat.format(date)
  }

  def httpDateTimeString(dt: DateTime): String = {
    httpDateTimeFormat.format(dt.getMillis)
  }
}

