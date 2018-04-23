package org.goldenport.values

import spire.math.Rational

/*
 * @since   Apr. 21, 2018
 * @version Apr. 22, 2018
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
}
