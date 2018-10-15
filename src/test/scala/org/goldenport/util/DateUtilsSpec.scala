package org.goldenport.util

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

/*
 * @since   Jun. 14, 2018
 * @version Jun. 14, 2018
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class DateUtilsSpec extends WordSpec with Matchers with GivenWhenThen {
  import SeqUtils._

  "parseYYYYMMDD" should {
    "20160319" in {
      DateUtils.parse2("20160319") should be(DateUtils.parse("2016-03-19"))
    }
  }
}
