package org.goldenport.util

import java.time.Instant
import org.joda.time.LocalDate
import org.joda.time.DateTimeZone

/*
 * @since   Jul. 22, 2025
 * @version Jul. 24, 2025
 * @author  ASAMI, Tomoharu
 */
object InstantUtils {
  implicit val instantOrderingAsc: Ordering[Instant] = Ordering.by(_.toEpochMilli)
  val instantOrderingDesc = instantOrderingAsc.reverse

  def toInstant(p: LocalDate): Instant = {
    val a = p.toDateTimeAtStartOfDay(DateTimeZone.UTC)
    a.toInstant.toDate.toInstant
  }
}
