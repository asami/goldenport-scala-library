package org.goldenport.util

import scala.util.Try
import scala.util.control.NonFatal
import java.util.{Date, Locale, TimeZone}
import java.sql.Timestamp
import java.text.SimpleDateFormat
import org.joda.time.{DateTime, LocalDate, LocalTime, DateTimeZone}
import org.joda.time.format.ISODateTimeFormat

/*
 * TODO unify org.goldenport.record.util.DateUtils
 *
 * @since   Jun. 17, 2014
 *  version Jul. 27, 2014
 *  version Aug. 14, 2014
 *  version Sep. 11, 2014
 *  version Aug. 27, 2015
 *  version Sep. 10, 2015
 *  version Feb.  3, 2016
 *  version Mar. 19, 2016
 *  version Sep. 23, 2016
 *  version Nov.  7, 2016
 *  version Aug. 29, 2017
 * @version May. 21, 2018
 * @author  ASAMI, Tomoharu
 */
object DateUtils {
  private val _gmt = TimeZone.getTimeZone("GMT")
  private val _jst = TimeZone.getTimeZone("JST")
  private lazy val _jodagmt = DateTimeUtils.jodagmt
  private lazy val _jodajst = DateTimeUtils.jodajst
  private val _df = new SimpleDateFormat("yyyy-MM-dd")
  _df.setTimeZone(_gmt) // java.util.Date holds date information as GMT in DB
  private val _df_yyyymmdd = new SimpleDateFormat("yyyyMMdd")
  _df_yyyymmdd.setTimeZone(_gmt) // java.util.Date holds date information as GMT in DB
  val isoFormatter = ISODateTimeFormat.date().withZoneUTC

  // Date contains GMT timezone value.
  def parse(s: String): Date = {
    synchronized {
      _df.parse(s.trim)
    }
  }

  def parseYYYYMMDD(s: String): Date = {
    synchronized {
      _df_yyyymmdd.parse(s.trim)
    }
  }

  def parseYYYYMMDD(s: Short): Date = parseYYYYMMDD(s.toString)
  def parseYYYYMMDD(s: Int): Date = parseYYYYMMDD(s.toString)
  def parseYYYYMMDD(s: Long): Date = parseYYYYMMDD(s.toString)

  def parse2(s: String): Date = {
    try {
      val x = s.replace('/', '-') // for excel YYYY/MM/DD
      parse(x)
    } catch {
      case NonFatal(e) => parseYYYYMMDD(s)
    }
  }

  def parse2Exclusive(s: String): Date = {
    val parsed = parse2(s)
    val localdate = toLocalDate(parsed)
    val nextdate = localdate.plusDays(1)
    val calced = nextdate.toDateTimeAtStartOfDay(_jodagmt)
    new Date(calced.getMillis)
  }

  // Date contains GMT timezone value.
  def toDateJst(v: Long): Date = {
    val a = new DateTime(v, _jodajst).toLocalDate.toDateTimeAtStartOfDay(_jodagmt)
    new Date(a.getMillis)
  }

  // Date contains GMT timezone value.
  def toDateJst(v: java.sql.Timestamp): Date = toDateJst(v.getTime)

  /**
   * GMT
   */
  def toIsoDateString(dt: java.sql.Date): String = {
    isoFormatter.print(dt.getTime)
  }

  /**
   * GMT
   */
  def toIsoDateString(dt: java.util.Date): String = {
    isoFormatter.print(dt.getTime)
  }

  def toDisplayString(d: DateTime): String = toDisplayString(d.toLocalDate)

  def toDisplayString(d: LocalDate): String = {
    f"${d.getYear}%02d/${d.getMonthOfYear}%02d/${d.getDayOfMonth}%02d"
  }

  // GMT
  def toYear(dt: java.util.Date): Int = {
    new DateTime(dt.getTime, DateTimeUtils.jodagmt).getYear
  }

  // GMT
  def toMonth(dt: java.util.Date): Int = {
    new DateTime(dt.getTime, DateTimeUtils.jodagmt).getMonthOfYear
  }

  // GMT
  def toDay(dt: java.util.Date): Int = {
    new DateTime(dt.getTime, DateTimeUtils.jodagmt).getDayOfMonth
  }

  def toYearMonthDay(dt: java.util.Date): (Int, Int, Int) = {
    val a = new DateTime(dt.getTime, DateTimeUtils.jodagmt)
    (a.getYear, a.getMonthOfYear, a.getDayOfMonth)
  }

  // Date(GMT) to Timestamp(JST)
  def toTimestampJst(date: java.util.Date): Timestamp = {
    toTimestamp(_jst, date)
  }

  // Date(GMT) to Timestamp(TimeZone)
  def toTimestamp(tz: TimeZone, date: java.util.Date): Timestamp = {
    new Timestamp(date.getTime + tz.getRawOffset)
  }

  // GMT
  def toLocalDate(date: java.util.Date): LocalDate = {
    val (y, m, d) = toYearMonthDay(date)
    new LocalDate(y, m, d)
  }

  // GMT
  def toLocalTime(date: java.util.Date): LocalTime = {
    new LocalTime(date.getTime)
  }

  /*
   * Logic
   */
  def calcAgeJst(date: java.util.Date): Int = {
    calcAge(_jst, date)
  }

  // GMT
  def calcAge(tz: TimeZone, date: java.util.Date): Int = {
    calcAge(tz, new Timestamp(System.currentTimeMillis), date)
  }

  def calcAge(tz: DateTimeZone, date: java.util.Date): Int = {
    calcAge(tz, new Timestamp(System.currentTimeMillis), date)
  }

  // GMT
  def calcAge(tz: TimeZone, base: Timestamp, date: java.util.Date): Int = {
    val ld = toLocalDate(date)
    val lb = TimestampUtils.toLocalDate(tz, base)
    LocalDateUtils.calcAge(lb, ld)
  }

  def calcAge(tz: DateTimeZone, base: Timestamp, date: java.util.Date): Int = {
    val ld = toLocalDate(date)
    val lb = TimestampUtils.toLocalDate(tz, base)
    LocalDateUtils.calcAge(lb, ld)
  }
}
