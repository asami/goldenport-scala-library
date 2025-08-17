package org.goldenport.values

import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest._
import org.goldenport.parser.ParseResultMatchers

/*
 * @since   Sep. 29, 2020
 *  version Jan. 23, 2021
 * @version Aug. 16, 2025
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class NumberIntervalSpec extends AnyWordSpec with Matchers with GivenWhenThen with ParseResultMatchers {
  "NumberInterval" should {
    "default-default" in {
      val r = NumberInterval.parse("100~200")
      r should parse_object(NumberInterval.closed(100, 200))
    }
    "default-open" in {
      val r = NumberInterval.parse("100~200)")
      r should parse_object(NumberInterval.openUpper(100, 200))
    }
    "default-close" in {
      val r = NumberInterval.parse("100~199]")
      r should parse_object(NumberInterval.closed(100, 199))
    }
  }
}
