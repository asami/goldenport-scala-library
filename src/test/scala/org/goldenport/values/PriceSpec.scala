package org.goldenport.values

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import spire.math.Rational

/*
 * @since   Jun. 29, 2018
 * @version Jun. 29, 2018
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class PriceSpec extends WordSpec with Matchers with GivenWhenThen {
  import Urn._

  "PriceIncludingTax" should {
    "createByPercent" in {
      PriceIncludingTax.createByPercent(BigDecimal(500), 8) should be(PriceIncludingTax(BigDecimal(500), Rational(1000, 27)))
    }
  }
}
