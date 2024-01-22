package org.goldenport.util

import scala.concurrent.duration._
import org.joda.time.{Duration => JodaDuration}
import org.joda.time.DateTimeConstants
import org.goldenport.parser._
import org.goldenport.extension.IRecord

/*
 * @since   Oct. 22, 2020
 *  version Feb. 14, 2021
 *  version Apr. 21, 2021
 * @version Oct. 29, 2022
 * @author  ASAMI, Tomoharu
 */
object DurationUtils {
  val MILLIS_PER_SECOND = DateTimeConstants.MILLIS_PER_SECOND
  val MILLIS_PER_MINUTE = DateTimeConstants.MILLIS_PER_MINUTE
  val MILLIS_PER_HOUR = DateTimeConstants.MILLIS_PER_HOUR
  val MILLIS_PER_DAY = DateTimeConstants.MILLIS_PER_DAY
  val MILLIS_PER_MONTH = MILLIS_PER_DAY * 30 // not accurate, approximate number
  val MILLIS_PER_YEAR = MILLIS_PER_MONTH * 12

  private val _regex = """D((\d+)Y)?((\d+)M)?((\d+)D)?(T((\d+)H)?((\d+)M)?((\d+)S)?)?""".r

  def isIn[T](base: Long, duration: Duration, f: T => Long)(p: T): Boolean =
    if (duration.isFinite)
      f(p) >= base - duration.toMillis
    else
      true

  def parseJoda(p: String): ParseResult[JodaDuration] = ParseResult.parse(
    _regex.findFirstMatchIn(p).map(m =>
      if (m.group(0) != p) {
        ParseResult.error(s"Not duration: $p")
      } else {
        for {
          year <- _to_int(m.group(2))
          month <- _to_int(m.group(4))
          day <- _to_int(m.group(6))
          hour <- _to_int(m.group(9))
          minute <- _to_int(m.group(11))
          second <- _to_int(m.group(13))
        } yield {
          val y = year * MILLIS_PER_YEAR
          val m = month * MILLIS_PER_MONTH
          val d = day * MILLIS_PER_DAY
          val h = hour * MILLIS_PER_HOUR
          val mi = minute * MILLIS_PER_MINUTE
          val s = second * MILLIS_PER_SECOND
          val millis = y + m + d + h + mi + s
          new JodaDuration(millis)
        }
      }
    ).getOrElse(ParseResult.error(s"Not period: $p"))
  )

  private def _to_int(p: String) = NumberUtils.parseInt(p, 0)

  def hour(h: Int): JodaDuration = JodaDuration.standardHours(h)

  def toRecord(p: FiniteDuration): IRecord = {
    val millis = p.toMillis
    val totalmillis = Some(millis)
    val vmillis = {
      val x = millis % 1000
      if (x == 0) None else Some(x)
    }
    val second = millis / 1000
    val vsecond = {
      val x = second % 60
      if (x == 0) None else Some(x)
    }
    val minute = second / 60
    val vminute = {
      val x = minute % 60
      if (x == 0) None else Some(x)
    }
    val hour = minute / 60
    val vhour = {
      val x = hour % 24
      if (x == 0) None else Some(x)
    }
    val day = hour / 24
    val vday = if (day == 0) None else Some(day)
    val cday = vday
    val chour = {
      if (vday.nonEmpty || vhour.nonEmpty)
        Some(vhour getOrElse 0)
      else
        None
    }
    val cminute = {
      if (vday.nonEmpty || vhour.nonEmpty || vminute.nonEmpty)
        Some(vminute getOrElse 0)
      else
        None
    }
    val csecond = {
      if (vday.nonEmpty || vhour.nonEmpty || vminute.nonEmpty || vsecond.nonEmpty)
        Some(vsecond getOrElse 0)
      else
        None
    }
    val cmillis = Some(vmillis getOrElse 0)
    val totalday = {
      if (vmillis.isEmpty && vsecond.isEmpty && vminute.isEmpty && vhour.isEmpty && vday.nonEmpty)
        Some(day)
      else
        None
    }
    val totalhour = {
      if (vmillis.isEmpty && vsecond.isEmpty && vminute.isEmpty && (vhour.nonEmpty || vday.nonEmpty))
        Some(hour)
      else
        None
    }
    val totalminute = {
      if (vmillis.isEmpty && vsecond.isEmpty && (vminute.nonEmpty || vhour.nonEmpty || vday.nonEmpty))
        Some(minute)
      else
        None
    }
    val totalsecond = {
      if (vmillis.isEmpty && (vsecond.nonEmpty || vminute.nonEmpty || vhour.nonEmpty || vday.nonEmpty))
        Some(second)
      else
        None
    }
    val text = {
      def _day_ = if (day == 1) s"${day} day" else s"${day} days"
      def _hour_ = if (hour == 1) s"${hour} hour" else s"${hour} hours"
      def _minute_ = if (minute == 1) s"${minute} minute" else s"${minute} minutes"
      def _second_ = if (second == 1) s"${second} second" else s"${second} seconds"
      def _millis_ = if (millis == 1) s"${millis} millisecond" else s"${millis} milliseconds"

      if (vmillis.isEmpty && vsecond.isEmpty && vminute.isEmpty && vhour.isEmpty && vday.nonEmpty)
        _day_
      else if (vmillis.isEmpty && vsecond.isEmpty && vminute.isEmpty && vhour.nonEmpty && vday.isEmpty)
        _hour_
      else if (vmillis.isEmpty && vsecond.isEmpty && vminute.nonEmpty && vhour.isEmpty && vday.isEmpty)
        _minute_
      else if (vmillis.isEmpty && vsecond.nonEmpty && vminute.isEmpty && vhour.isEmpty && vday.isEmpty)
        _second_
      else
        _millis_
    }
    IRecord.data(
      "text" -> text,
      "composition" -> IRecord.dataOption(
        "day" -> cday,
        "hour" -> chour,
        "minute" -> cminute,
        "second" -> csecond,
        "millis" -> cmillis
      )
    ) + IRecord.dataOption(
      "day" -> totalday,
      "hour" -> totalhour,
      "minute" -> totalminute,
      "second" -> totalsecond,
      "millis" -> totalmillis
    )
  }
}
