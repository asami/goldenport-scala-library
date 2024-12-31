package org.goldenport.util

import scala.util.{Try, Success}
import java.util.{Date, TimeZone, Locale}
import java.sql.Timestamp
import java.text.SimpleDateFormat
import org.joda.time._
import org.joda.time.format.{ISODateTimeFormat, DateTimeFormat}
import org.goldenport.RAISE
import org.goldenport.context.Consequence
import org.goldenport.context.{DateTimeContext => LibDateTimeContext}
import org.goldenport.extension.IRecord
import org.goldenport.i18n.LocaleUtils

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
 *  version Dec. 29, 2018
 *  version Sep. 25, 2019
 *  version Jan. 27, 2022
 *  version Feb. 19, 2022
 *  version Apr. 20, 2022
 *  version May.  2, 2022
 *  version Jun. 17, 2022
 *  version Nov. 17, 2022
 *  version Dec. 28, 2022
 * @version Oct. 14, 2024
 * @author  ASAMI, Tomoharu
 */
object DateTimeUtils {
  val gmt = TimeZone.getTimeZone("GMT")
  val jst = TimeZone.getTimeZone("JST")
  val jodagmt = DateTimeZone.forID("GMT")
  val jodajst = DateTimeZone.forID("Asia/Tokyo")
  val jodaplus900 = DateTimeZone.forOffsetHours(9)
  val jodaEst = DateTimeZone.forID("EST")
  val jodaEuropeBerlin = DateTimeZone.forID("Europe/Berlin")
  val jodaEuropeZurich = DateTimeZone.forID("Europe/Zurich")
  val isoFormatter = ISODateTimeFormat.dateTimeNoMillis() // ??? NoMillis
  val isoUtcFormatter = isoFormatter.withZoneUTC
  val isoJstFormatter = isoFormatter.withZone(jodajst)
  val isoJstParser = ISODateTimeFormat.dateTimeParser.withZone(jodajst)
  val isoNoMillisFormatter = ISODateTimeFormat.dateTimeNoMillis()
  val isoNoMillisUtcFormatter = isoNoMillisFormatter.withZoneUTC
  val isoNoMillisJstFormatter = isoNoMillisFormatter.withZone(jodajst)
  val basicFormattter = ISODateTimeFormat.basicDateTime.withZoneUTC // yyyyMMdd'T'HHmmss.SSSZ
  val basicFormattterJst = basicFormattter.withZone(jodajst)
  val basicNoMillisFormattter = ISODateTimeFormat.basicDateTimeNoMillis
  val simpleFormatter = DateTimeFormat.forPattern("yyyyMMdd HHmmss").withZoneUTC
  val simpleFormatterJst = simpleFormatter.withZone(jodajst)
  val httpDateTimeFormat = {
    // http://candrews.integralblue.com/2009/02/http-caching-header-aware-servlet-filter/
    val sdf = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss z", Locale.US)
    sdf.setTimeZone(gmt)
    sdf
  }

  def toIsoDateTimeString(dt: DateTime): String =
    toIsoDateTimeString(dt, dt.getZone)

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

  def toIsoDateTimeNoMillisString(dt: DateTime): String =
    toIsoDateTimeNoMillisString(dt, dt.getZone)

  def toIsoDateTimeNoMillisString(dt: DateTime, tz: DateTimeZone): String =
    toIsoDateTimeNoMillisString(dt.getMillis, tz)

  def toIsoDateTimeNoMillisString(dt: Long, tz: DateTimeZone): String = {
    val fmt = if (tz == jodajst)
      isoNoMillisJstFormatter
    else if (tz == jodagmt)
      isoNoMillisUtcFormatter
    else
      isoNoMillisFormatter.withZone(tz)
    fmt.print(dt)
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
      case (Some(s), None) => Some(s"${dt(s)} 〜")
      case (None, Some(e)) => Some(s"〜 ${dt(e)}")
      case (None, None) => None
    }
  }

  def toSimpleStringGmt(dt: DateTime): String = simpleFormatter.print(dt)

  def toSimpleStringGmt(ts: java.sql.Timestamp): String = simpleFormatter.print(ts.getTime)

  def toSimpleStringJst(dt: DateTime): String = simpleFormatterJst.print(dt)

  def toSimpleStringJst(ts: java.sql.Timestamp): String = simpleFormatterJst.print(ts.getTime)

  def toSimpleString24Jst(dt: java.sql.Timestamp): String = RAISE.notImplementedYetDefect

  def toDisplayString(p: DateTime): String =
    if (hasMillisPart(p))
      toIsoDateTimeString(p)
    else
      toIsoDateTimeNoMillisString(p)

  // Parser
  def parseIsoDateTime(s: String, tz: DateTimeZone): DateTime = {
    val a = Try(
      // if (tz == jodajst)
      //   isoJstParser.withOffsetParsed().parseDateTime(s)
      // else
      //   ISODateTimeFormat.dateTimeParser.withOffsetParsed().withZone(tz).parseDateTime(s)
      ISODateTimeFormat.dateTimeParser.withOffsetParsed().withZone(tz).parseDateTime(s)
    )
    a match {
      case Success(s) => s
      case _ =>
        val b = a.recover {
          case e => LocalDateTime.parse(s).toDateTime(tz)
        }.recover {
          case e => LocalDate.parse(s).toDateTimeAtStartOfDay(tz)
        }
        b match {
          case Success(s) => s
          case _ => a.get
        }
    }
  }

  def parseIsoDateTimeJst(s: String): DateTime = {
    parseIsoDateTime(s, jodajst)
  }

  def parseDateTime(s: String, tz: DateTimeZone): DateTime = {
    val r = ".*T.*[+-].*".r
    if (r.findFirstMatchIn(s).isEmpty)
      parseIsoDateTime(s, tz)
    else
      _parse_datetime(s)
  }

  private def _parse_datetime(s: String): DateTime = 
    ISODateTimeFormat.dateTimeParser.withOffsetParsed().parseDateTime(s)

  def parseDateTimeJst(s: String): DateTime = {
    parseDateTime(s, jodajst)
  }

  def consequenceDateTimeWithContext(s: String)(implicit ctx: org.goldenport.context.DateTimeContext): Consequence[DateTime] =
    consequenceDateTime(s, ctx.dateTimeZone)

  def consequenceDateTime(s: String): Consequence[DateTime] =
    consequenceDateTime(s, jodajst) // TODO

  def consequenceDateTime(s: String, tz: DateTimeZone): Consequence[DateTime] = Consequence(
    parseDateTime(s, tz)
  )

  def makeForFormatting(p: String): Any = {
    val a = Try(LocalDateTimeUtils.parse(p)) orElse Try(LocalDateUtils.parse2(p))
    a getOrElse parseDateTime(p, jodajst)
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

  def toDateTime(dt: java.util.Date, tz: TimeZone): DateTime =
    toDateTime(dt.getTime, tz)

  def toDateTime(dt: java.sql.Timestamp, tz: TimeZone): DateTime = {
    toDateTime(dt.getTime, tz)
  }

  def toDateTime(dt: Long, tz: TimeZone): DateTime = {
    new DateTime(dt, tzToDateTimeZone(tz))
  }

  def toDateTime(dt: DateTime, tz: TimeZone): DateTime = {
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

  def toCalendar(p: DateTime): java.util.Calendar = p.toCalendar(Locale.US)

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

  /*
   * Record
   */
  def toRecord(p: DateTime, ctx: LibDateTimeContext): IRecord = toRecord(p, ctx.dateTimeZone)

  def toRecord(p: DateTime, tz: DateTimeZone): IRecord = toRecord(p.withZone(tz))

  def toRecord(p: DateTime): IRecord = {
    val zone = p.getZone.toTimeZone
    val ut = p.getMillis
    val offset = zone.getOffset(ut)
    val osign = if (offset >= 0) "+" else "-"
    val aoffset = scala.math.abs(offset) / 1000
    val ohour = aoffset / (60 * 60)
    val ominute = aoffset % (60 * 60) / 60
    val omark = f"$osign$ohour%02d$ominute%02d"
    IRecord.data(
      "year" -> p.getYear,
      "month" -> p.getMonthOfYear,
      "day" -> p.getDayOfMonth,
      "hour" -> p.getHourOfDay,
      "minute" -> p.getMinuteOfHour,
      "second" -> p.getSecondOfMinute,
      "millis" -> p.getMillisOfSecond,
      "timezone" -> IRecord.data(
        "ID" -> zone.getID,
        "name" -> zone.getDisplayName(false, TimeZone.LONG, LocaleUtils.C),
        "short-name" -> zone.getDisplayName(false, TimeZone.SHORT, LocaleUtils.C),
        "offset" -> IRecord.data(
          "mark" -> omark,
          "sign" -> osign,
          "hour" -> ohour,
          "minute" -> ominute
        )
      )
    )
  }

  /*
   * DateTimeZone
   */
  def tzToDateTimeZone(p: TimeZone): DateTimeZone = DateTimeZone.forTimeZone(p)

  def dateTimeZoneToTz(p: DateTimeZone): TimeZone = p.toTimeZone

  /*
   * Calculation
   */
  def startOfFirstDayOfNextMonth(p: DateTime): DateTime =
    p.plusMonths(1).withDayOfMonth(1).withTimeAtStartOfDay

  def startOfFirstDayOfThisMonth(p: DateTime): DateTime =
    p.withDayOfMonth(1).withTimeAtStartOfDay

  def endOfLastDayOfThisMonth(p: DateTime): DateTime =
    startOfFirstDayOfNextMonth(p).minusMillis(1)

  def startOfFirstDayOfNextYear(p: DateTime): DateTime =
    p.plusYears(1).withDayOfMonth(1).withDayOfYear(1).withTimeAtStartOfDay

  def startOfFirstDayOfThisYear(p: DateTime): DateTime =
    p.withDayOfMonth(1).withDayOfYear(1).withTimeAtStartOfDay

  def endOfLastDayOfThisYear(p: DateTime): DateTime =
    startOfFirstDayOfNextYear(p).minusMillis(1)

  def startOfToday(p: DateTime): DateTime = p.withTimeAtStartOfDay

  /*
   * Count
   */
  def countOfMonthsAlmost(start: DateTime, end: DateTime): Int =
    LocalDateTimeUtils.countOfMonthsAlmost(start.toLocalDateTime, end.withZone(start.getZone).toLocalDateTime)

  def countOfMonthsPassed(start: DateTime, end: DateTime): Int =
    LocalDateTimeUtils.countOfMonthsPassed(start.toLocalDateTime, end.withZone(start.getZone).toLocalDateTime)

  def countOfYearsAlmost(start: DateTime, end: DateTime): Int =
    LocalDateTimeUtils.countOfYearsAlmost(start.toLocalDateTime, end.withZone(start.getZone).toLocalDateTime)

  def countOfYearsPassed(start: DateTime, end: DateTime): Int =
    LocalDateTimeUtils.countOfYearsPassed(start.toLocalDateTime, end.withZone(start.getZone).toLocalDateTime)

  //
  def isSameMonth(lhs: DateTime, rhs: DateTime): Boolean =
    LocalDateUtils.isSameMonth(lhs.toLocalDate, rhs.toLocalDate)

  def hasMillisPart(p: DateTime): Boolean = p.getMillis % 1000 != 0
}

