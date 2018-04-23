package org.goldenport.util

import org.joda.time._

/*
 * @since   Apr. 12, 2018
 * @version Apr. 21, 2018
 * @author  ASAMI, Tomoharu
 */
object SpireUtils {
  object Implicits {
    implicit object BigDeimalOrder extends spire.algebra.Order[BigDecimal] {
      def compare(l: BigDecimal, r: BigDecimal) = l.compare(r)
    }
    implicit object DateTimeOrder extends spire.algebra.Order[DateTime] {
      def compare(l: DateTime, r: DateTime) = l.getMillis.compare(r.getMillis)
    }
    implicit object LocalTimeOrder extends spire.algebra.Order[LocalTime] {
      def compare(l: LocalTime, r: LocalTime) = l.getMillisOfDay.compare(r.getMillisOfDay)
    }
  }
}
