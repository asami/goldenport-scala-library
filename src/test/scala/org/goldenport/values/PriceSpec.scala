package org.goldenport.values

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import spire.math.Rational

/*
 * @since   Jun. 29, 2018
 * @version Dec. 22, 2020
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class PriceSpec extends WordSpec with Matchers with GivenWhenThen {
  import Urn._

  "PriceIncludingTax" should {
    "createByPercent" in {
      PriceIncludingTax.createByPercent(BigDecimal(500), 8) should be(PriceIncludingTax(BigDecimal(500), Rational(1000, 27)))
    }
    "calc 1" in {
      val price = PriceIncludingTax.createByPercent(9000, 10)
      println(s"calc: ${price.taxRational.toFloat}")
      val x = BigDecimal(9000)
      val rate = BigDecimal(0.1)
      val a = (x / (1 + rate)) * rate
      val mc = java.math.MathContext.DECIMAL32
      println(s"calc BigDecimal: ${a.round(mc)}")
    }
    "calc 2" in {
      val price = PriceIncludingTax.createByPercent(500, 10)
      println(s"calc: ${price.taxRational.toFloat}")
    }
  }
}
