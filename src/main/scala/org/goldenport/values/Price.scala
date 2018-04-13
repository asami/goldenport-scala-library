package org.goldenport.values

import scalaz._, Scalaz._

/*
 * @since   Apr. 12, 2018
 * @version Apr. 13, 2018
 * @author  ASAMI, Tomoharu
 */
sealed trait Price {
  def isPriceExcluded: Boolean
  def priceIncludedTax: BigDecimal
  def priceExcludedTax: BigDecimal
  def tax: BigDecimal
}
object Price {
  object Implicits {
    implicit object PriceExcludedTaxMonoid extends Monoid[PriceExcludedTax] {
      def zero = PriceExcludedTax.zero
      def append(l: PriceExcludedTax, r: => PriceExcludedTax): PriceExcludedTax = l + r
    }

    implicit object PriceIncludedTaxMonoid extends Monoid[PriceIncludedTax] {
      def zero = PriceIncludedTax.zero
      def append(l: PriceIncludedTax, r: => PriceIncludedTax): PriceIncludedTax = l + r
    }
  }
}

case class PriceExcludedTax(price: BigDecimal, tax: BigDecimal) extends Price {
  def isPriceExcluded = true
  def priceIncludedTax = price + tax
  def priceExcludedTax = price
  def toIncludedTax: PriceIncludedTax = PriceIncludedTax(price + tax, tax)

  def +(rhs: PriceExcludedTax): PriceExcludedTax =
    PriceExcludedTax(price + rhs.price, tax + rhs.tax)
}
object PriceExcludedTax {
  val zero = PriceExcludedTax(BigDecimal(0), BigDecimal(0))
}

case class PriceIncludedTax(price: BigDecimal, tax: BigDecimal) extends Price {
  def isPriceExcluded = false
  def priceIncludedTax = price
  def priceExcludedTax = price - tax
  def toExcludedTax: PriceExcludedTax = PriceExcludedTax(price - tax, tax)

  def +(rhs: PriceIncludedTax): PriceIncludedTax =
    PriceIncludedTax(price + rhs.price, tax + rhs.tax)
}
object PriceIncludedTax {
  val zero = PriceIncludedTax(BigDecimal(0), BigDecimal(0))
}
