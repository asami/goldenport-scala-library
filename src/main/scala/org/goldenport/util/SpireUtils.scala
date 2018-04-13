package org.goldenport.util

/*
 * @since   Apr. 12, 2018
 * @version Apr. 12, 2018
 * @author  ASAMI, Tomoharu
 */
object SpireUtils {
  object Implicits {
    implicit object BigDeimalOrder extends spire.algebra.Order[BigDecimal] {
      def compare(l: BigDecimal, r: BigDecimal) = l.compare(r)
    }
  }
}

