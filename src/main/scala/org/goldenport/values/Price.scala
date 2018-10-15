package org.goldenport.values

import scalaz._, Scalaz._
import java.math.MathContext
import spire.math.Rational

/*
 * @since   Apr. 12, 2018
 *  version Apr. 22, 2018
 * @version Jun. 29, 2018
 * @author  ASAMI, Tomoharu
 */
sealed trait Price {
  def displayPrice: BigDecimal
  def isTaxExclusive: Boolean
  def priceIncludingTax: BigDecimal
  def priceExcludingTax: BigDecimal
  def taxRational: Rational
  def mathContext: MathContext
  lazy val tax = taxRational.toBigDecimal(mathContext)

  // def toMap: Map[String, BigDecimal] = Map.empty // TODO
    // protected def to_record(p: Price): Record = {
    //   displayprice = p match {
    //     case m: PriceExcludingTax => m.priceExcludingTax
    //     case m: PriceIncludingTax => m.priceIncludingTax
    //   }
    //   Record.dataApp(
    //     "displayPrice" -> displayPrice,
    //     "isTaxExclusive" -> p.isPriceExcluding,
    //     "tax" -> p.tax,
    //     "priceExcludingTax" -> p.priceExcludingTax,
    //     "priceIncludingTax" -> p.priceIncludingTax
    //   )
    // }
}
object Price {
  implicit object PriceExcludingTaxMonoid extends Monoid[PriceExcludingTax] {
    def zero = PriceExcludingTax.ZERO
    def append(l: PriceExcludingTax, r: => PriceExcludingTax): PriceExcludingTax = l + r
  }

  implicit object PriceIncludingTaxMonoid extends Monoid[PriceIncludingTax] {
    def zero = PriceIncludingTax.ZERO
    def append(l: PriceIncludingTax, r: => PriceIncludingTax): PriceIncludingTax = l + r
  }

  implicit object PriceNoTaxMonoid extends Monoid[PriceNoTax] {
    def zero = PriceNoTax.ZERO
    def append(l: PriceNoTax, r: => PriceNoTax): PriceNoTax = l + r
  }
}

case class PriceExcludingTax(
  price: BigDecimal,
  taxRational: Rational,
  mathContext: MathContext = MathContext.DECIMAL32
) extends Price {
  def displayPrice = price
  def isTaxExclusive = true
  def priceIncludingTax = price + tax
  def priceExcludingTax = price
  def toIncludingTax: PriceIncludingTax = PriceIncludingTax(price + tax, tax)

  def +(rhs: PriceExcludingTax): PriceExcludingTax =
    PriceExcludingTax(price + rhs.price, tax + rhs.tax)
}
object PriceExcludingTax {
  val ZERO = PriceExcludingTax(BigDecimal(0), 0)

  def createByPercent(p: BigDecimal, percent: Int): PriceExcludingTax =
    createByRate(p, percent * Rational(1, 100))

  def createByRate(p: BigDecimal, rate: Rational): PriceExcludingTax = 
    PriceExcludingTax(p, p * rate)
}

case class PriceIncludingTax(
  price: BigDecimal,
  taxRational: Rational,
  mathContext: MathContext = MathContext.DECIMAL32
) extends Price {
  def displayPrice = price
  def isTaxExclusive = false
  def priceIncludingTax = price
  def priceExcludingTax = price - tax
  def toExcludingTax: PriceExcludingTax = PriceExcludingTax(price - tax, tax)

  def +(rhs: PriceIncludingTax): PriceIncludingTax =
    PriceIncludingTax(price + rhs.price, tax + rhs.tax)
}
object PriceIncludingTax {
  val ZERO = PriceIncludingTax(BigDecimal(0), 0)

  def createByPercent(p: BigDecimal, percent: Int): PriceIncludingTax =
    createByRate(p, calcTaxRateRationalByPercent(percent))

  def createByRate(p: BigDecimal, rate: Rational): PriceIncludingTax = {
    val tax = (p / (1 + rate)) * rate
    PriceIncludingTax(p, tax)
  }

  def calcTaxRateRationalByPercent(percent: Int): Rational =
    percent * Rational(1, 100)
}

case class PriceNoTax(price: BigDecimal) extends Price {
  val mathContext: MathContext = MathContext.DECIMAL32
  def displayPrice = price
  def isTaxExclusive = false
  def priceIncludingTax = price
  def priceExcludingTax = price
  def taxRational = 0

  def +(rhs: PriceNoTax): PriceNoTax =
    PriceNoTax(price + rhs.price)
}
object PriceNoTax {
  val ZERO = PriceNoTax(BigDecimal(0))
}
