package org.goldenport.values

import spire.math.Rational
import org.goldenport.util.NumberUtils

/*
 * @since   Apr. 21, 2018
 *  version Apr. 22, 2018
 * @version Oct.  9, 2024
 * @author  ASAMI, Tomoharu
 */
sealed trait Percent {
  def rate: Rational
}
case object ZeroPercent extends Percent {
  val rate = Rational(0)
}
case class IntPercent(v: Int) extends Percent {
  lazy val rate: Rational = Rational(v) / 100
}
case class LongPercent(v: Long) extends Percent {
  lazy val rate: Rational = Rational(v) / 100
}
case class FloatPercent(v: Float) extends Percent {
  lazy val rate: Rational = Rational(v) / 100
}
case class DoublePercent(v: Double) extends Percent {
  lazy val rate: Rational = Rational(v) / 100
}
case class BigIntPercent(v: BigInt) extends Percent {
  lazy val rate: Rational = Rational(v) / 100
}
case class BigDecimalPercent(v: BigDecimal) extends Percent {
  lazy val rate: Rational = Rational(v) / 100
}
case class RationalPercent(v: Rational) extends Percent {
  lazy val rate: Rational = v / 100
}
object Percent {
  val ZERO = ZeroPercent

  def apply(p: Int): Percent = IntPercent(p)
  def apply(p: Long): Percent = LongPercent(p)
  def apply(p: Float): Percent = FloatPercent(p)
  def apply(p: Double): Percent = DoublePercent(p)
  def apply(p: BigInt): Percent = BigIntPercent(p)
  def apply(p: BigDecimal): Percent = BigDecimalPercent(p)
  def apply(p: String): Percent = BigDecimalPercent(BigDecimal(p))
  def apply(p: Rational): Percent = RationalPercent(p)
  def apply(p: Number): Percent = p match {
    case m: java.lang.Integer => IntPercent(m)
    case m: java.lang.Long => LongPercent(m)
    case m: java.lang.Float => FloatPercent(m)
    case m: java.lang.Double => DoublePercent(m)
    case m: java.math.BigInteger => BigIntPercent(m)
    case m: BigDecimal => BigDecimalPercent(m)
    case m: Rational => RationalPercent(m)
    case m => BigDecimalPercent(NumberUtils.toBigDecimal(m))
  }
}
