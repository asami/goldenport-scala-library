package org.goldenport.util

import org.joda.time._

/*
 * @since   Apr. 12, 2018
 *  version Apr. 21, 2018
 *  version Jan. 21, 2021
 *  version Apr. 22, 2021
 * @version Oct. 13, 2024
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

  val ZERO = spire.math.Number(0)
  val ONE = spire.math.Number(1)

  def toJavaNumber(p: spire.math.Number): Number =
    if (p.canBeInt)
      p.intValue
    else if (p.canBeLong)
      p.longValue
    else if (p.withinDouble)
      p.doubleValue
    else
      p.toBigDecimal.bigDecimal

  def toSpireNumber(p: Number): spire.math.Number = p match {
    case n: java.lang.Integer    => spire.math.Number(n.intValue())
    case n: java.lang.Long       => spire.math.Number(n.longValue())
    case n: java.lang.Float      => spire.math.Number(n.floatValue())
    case n: java.lang.Double     => spire.math.Number(n.doubleValue())
    case n: java.math.BigDecimal => spire.math.Number(n)
    case n: java.math.BigInteger => spire.math.Number(n)
    case n: scala.math.BigDecimal => spire.math.Number(n)
    case n: scala.math.BigInt     => spire.math.Number(n)
    case _ => throw new IllegalArgumentException("Unsupported number type")
  }
}
