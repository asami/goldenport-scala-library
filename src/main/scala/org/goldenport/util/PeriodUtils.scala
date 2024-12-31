package org.goldenport.util

import org.joda.time._
import org.goldenport.parser._

/*
 * @since   Feb. 14, 2021
 *  version Apr. 21, 2021
 * @version Oct.  9, 2024
 * @author  ASAMI, Tomoharu
 */
object PeriodUtils {
  private val _regex = """P((\d+)Y)?((\d+)M)?((\d+)D)?(T((\d+)H)?((\d+)M)?((\d+)S)?)?""".r

  def parse(p: String): ParseResult[Period] = ParseResult.parse(
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
        } yield new Period(year, month, 0, day, hour, minute, second, 0)
      }
    ).getOrElse(ParseResult.error(s"Not period: $p"))
  )

  private def _to_int(p: String) = NumberUtils.parseInt(p, 0)

  def day(d: Int): Period = new Period().withDays(d)

  def yearMonthDay(y: Int, m: Int, d: Int): Period = new Period(y, m, 0, d, 0, 0, 0)
}
