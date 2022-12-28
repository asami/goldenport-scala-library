package org.goldenport.util

import scala.util.control.NonFatal
import com.github.nscala_time.time.Imports._
import org.goldenport.context.Consequence

/*
 * @since   Dec. 12, 2022
 * @version Dec. 28, 2022
 * @author  ASAMI, Tomoharu
 */
object LocalTimeUtils {
  val isoNoMillisFormatter = DateTimeFormat.forPattern("HH:mm:ss")

  def toDisplayString(p: LocalTime): String = isoNoMillisFormatter.print(p)

  def parse(s: String): LocalTime = LocalTime.parse(s.trim)

  def consequenceLocalTime(p: String): Consequence[LocalTime] =
    Consequence(parse(p))
}
