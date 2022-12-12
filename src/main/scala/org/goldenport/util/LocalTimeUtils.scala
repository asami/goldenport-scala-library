package org.goldenport.util

import scala.util.control.NonFatal
import com.github.nscala_time.time.Imports._
import org.goldenport.context.Consequence

/*
 * @since   Dec. 12, 2022
 * @version Dec. 12, 2022
 * @author  ASAMI, Tomoharu
 */
object LocalTimeUtils {
  def parse(s: String): LocalTime = LocalTime.parse(s.trim)

  def consequenceLocalTime(p: String): Consequence[LocalTime] =
    Consequence(parse(p))
}
