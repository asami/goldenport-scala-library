package org.goldenport.util

import scala.util.Try
import org.joda.time.LocalTime

/*
 * @since   Dec. 10, 2022
 * @version Dec. 10, 2022
 * @author  ASAMI, Tomoharu
 */
object TimeUtils {
  def makeForFormatting(p: String): Any = {
    val a = Try(LocalDateTimeUtils.parse(p)) orElse Try(LocalTime.parse(p))
    a getOrElse DateTimeUtils.parseDateTime(p, DateTimeUtils.jodagmt)
  }
}
