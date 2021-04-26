package org.goldenport.util

import org.joda.time._

/*
 * @since   Apr. 12, 2018
 *  version Apr. 21, 2018
 *  version Jan. 21, 2021
 * @version Apr. 22, 2021
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
    implicit object LocalDateTimeOrder extends spire.algebra.Order[LocalDateTime] {
      def compare(l: LocalDateTime, r: LocalDateTime) = l.compareTo(r)
    }
    implicit object LocalTimeOrder extends spire.algebra.Order[LocalTime] {
      def compare(l: LocalTime, r: LocalTime) = l.getMillisOfDay.compare(r.getMillisOfDay)
    }
  }

  def toJavaNumber(p: spire.math.Number): Number =
    if (p.canBeInt)
      p.intValue
    else if (p.canBeLong)
      p.longValue
    else if (p.withinDouble)
      p.doubleValue
    else
      p.toBigDecimal.bigDecimal
}

