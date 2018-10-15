package org.goldenport.util

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import org.joda.time.LocalDate

/*
 * @since   Jun. 14, 2018
 * @version Jun. 14, 2018
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class LocalDateUtilsSpec extends WordSpec with Matchers with GivenWhenThen {
  import SeqUtils._

  "parseYYYYMMDD" should {
    "20160319" in {
      LocalDateUtils.parse2("20160319") should be(new LocalDate("2016-03-19"))
    }
  }
}
