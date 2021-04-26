package org.goldenport.util

import scala.concurrent.duration._
import org.joda.time.{Duration => JodaDuration}
import org.joda.time.DateTimeConstants
import org.goldenport.parser._

/*
 * @since   Oct. 22, 2020
 *  version Feb. 14, 2021
 * @version Apr. 21, 2021
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
}
