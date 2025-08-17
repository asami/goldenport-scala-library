package org.goldenport.util

import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest._

/*
 * @since   Jun. 14, 2018
 * @version Aug. 16, 2025
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class DateUtilsSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  import SeqUtils._

  "parseYYYYMMDD" should {
    "20160319" in {
      DateUtils.parse2("20160319") should be(DateUtils.parse("2016-03-19"))
    }
  }
}
