package org.goldenport.util

import scala.concurrent.duration._
import org.joda.time.{Duration => JodaDuration}
import org.goldenport.parser._

/*
 * @since   Oct. 22, 2020
 * @version Feb. 14, 2021
 * @author  ASAMI, Tomoharu
 */
object DurationUtils {
  private val _regex = """D((\d+)Y)?((\d+)M)?((\d+)D)?(T((\d+)H)?((\d+)M)?((\d+)S)?)?""".r

  def isIn[T](base: Long, duration: Duration, f: T => Long)(p: T): Boolean =
    if (duration.isFinite)
      f(p) >= base - duration.toMillis
    else
      true

  def parseJoda(p: String): ParseResult[JodaDuration] = ParseResult.parse(
    _regex.findFirstMatchIn(p).map(m =>
      if (m.group(0) != p) {
        ParseResult.error(s"Not period: $p")
      } else {
        for {
          year <- _to_int(m.group(2))
          month <- _to_int(m.group(4))
          day <- _to_int(m.group(6))
          hour <- _to_int(m.group(9))
          minute <- _to_int(m.group(11))
          second <- _to_int(m.group(13))
        } yield new JodaDuration(year, month, day, hour, minute, second, 0)
      }
    ).getOrElse(ParseResult.error(s"Not period: $p"))
  )

  private def _to_int(p: String) = NumberUtils.parseInt(p, 0)

  def hour(h: Int): JodaDuration = JodaDuration.standardHours(h)
}
