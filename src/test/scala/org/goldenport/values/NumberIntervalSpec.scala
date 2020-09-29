package org.goldenport.values

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import org.goldenport.parser.ParseResultMatchers

/*
 * @since   Sep. 29, 2020
 * @version Sep. 29, 2020
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class NumberIntervalSpec extends WordSpec with Matchers with GivenWhenThen with ParseResultMatchers {
  "NumberInterval" should {
    "default-default" in {
      val r = NumberInterval.parse("100~200")
      r should parse_object(NumberInterval.openUpper(100, 200))
    }
    "default-close" in {
      val r = NumberInterval.parse("100~199]")
      r should parse_object(NumberInterval.closed(100, 199))
    }
  }
}
