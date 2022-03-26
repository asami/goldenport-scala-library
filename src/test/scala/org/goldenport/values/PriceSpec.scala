package org.goldenport.values

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import spire.math.Rational

/*
 * @since   Jun. 29, 2018
 *  version Dec. 22, 2020
 * @version Mar. 26, 2022
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class PriceSpec extends WordSpec with Matchers with GivenWhenThen {
  import Urn._

  "PriceIncludingTax" should {
    "createByPercent" in {
      PriceIncludingTax.createByPercent(BigDecimal(500), 8) should be(PriceIncludingTax(BigDecimal(500), Rational(2, 25)))
    }
    "calc 1" in {
      val price = PriceIncludingTax.createByPercent(9000, 10)
//      println(s"calc: ${price.taxRational.toFloat}")
      val x = BigDecimal(9000)
      val rate = BigDecimal(0.1)
      val a = (x / (1 + rate)) * rate
      val mc = java.math.MathContext.DECIMAL32
//      println(s"calc BigDecimal: ${a.round(mc)}")
    }
    "calc 2" in {
      val price = PriceIncludingTax.createByPercent(500, 10)
//      println(s"calc: ${price.taxRational.toFloat}")
    }
  }

  "Marshalling" should {
    "PriceIncludingTax" in {
      val x = PriceIncludingTax.createByPercent(BigDecimal(500), 8)
      val s = x.marshall
//      println(s)
      val v = Price.unmarshall(s)
      v.take should be(x)
    }
    "PriceIncludingTax Old" in {
      val s = """{
    "kind" : "price-including-tax",
    "price" : "500",
    "tax" : "1000/27"
}"""
      val x = PriceIncludingTax.createByPercent(BigDecimal(500), 8)
      val v = Price.unmarshall(s)
      v.take should be(x)
    }
    "PriceExcludingTax" in {
      val x = PriceExcludingTax.createByPercent(BigDecimal(500), 8)
      val s = x.marshall
//      println(s)
      val v = Price.unmarshall(s)
      v.take should be(x)
    }
  }

  "TaxRate" should {
    "PriceIncludingTax" which {
      "createByRate" in {
        val price = PriceIncludingTax.createByRate(10000, 0.1)
        // println(price)
        price.taxRate should be(BigDecimal(0.1))
      }
      "createByTax" in {
        val price = PriceIncludingTax.createByTax(11100, 1000)
        println(price)
        price.taxRate should be(BigDecimal(0.1))
      }
    }
    "PriceExcludingTax" which {
      "createByRate" in {
        val price = PriceExcludingTax.createByRate(1000, 0.1)
        price.taxRate should be(BigDecimal(0.1))
      }
    }
  }
}
