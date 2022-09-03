package org.goldenport.values

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

/*
 * @since   May. 19, 2022
 *  version May. 20, 2022
 * @version Aug. 25, 2022
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class DateTimePeriodSpec extends WordSpec with Matchers with GivenWhenThen {
  def datetimeperiod(s: String) = DateTimePeriod.parse(s)

  "parse" should {
    "parse" which {
      "parse" in {
        val x = datetimeperiod("~2022-09-01T00:00:00)")
        println(x)
      }
    }
  }
  "<" should {
    "<" which {
      "open-close" in {
        val l = datetimeperiod("[2022-05-20T10:00:00~2022-05-20T11:00:00)")
        val r = datetimeperiod("[2022-05-20T11:00:00~2022-05-20T12:00:00)")
        l < r should be(true)
      }
      "close-close" in {
        val l = datetimeperiod("[2022-05-20T10:00:00~2022-05-20T11:00:00]")
        val r = datetimeperiod("[2022-05-20T11:00:00~2022-05-20T12:00:00)")
        l < r should be(false)
      }
      "same" in {
        val l = datetimeperiod("[2022-05-20T10:00:00~2022-05-20T11:00:00)")
        val r = datetimeperiod("[2022-05-20T10:00:00~2022-05-20T11:00:00)")
        l < r should be(false)
      }
      "reverse" in {
        val l = datetimeperiod("[2022-05-20T11:00:00~2022-05-20T12:00:00)")
        val r = datetimeperiod("[2022-05-20T10:00:00~2022-05-20T11:00:00)")
        l < r should be(false)
      }
    }
    ">" which {
      "open-close" in {
        val l = datetimeperiod("[2022-05-20T11:00:00~2022-05-20T12:00:00)")
        val r = datetimeperiod("[2022-05-20T10:00:00~2022-05-20T11:00:00)")
        l > r should be(true)
      }
      "close-close" in {
        val l = datetimeperiod("[2022-05-20T11:00:00~2022-05-20T12:00:00)")
        val r = datetimeperiod("[2022-05-20T10:00:00~2022-05-20T11:00:00]")
        l > r should be(false)
      }
      "same" in {
        val l = datetimeperiod("[2022-05-20T10:00:00~2022-05-20T11:00:00)")
        val r = datetimeperiod("[2022-05-20T10:00:00~2022-05-20T11:00:00)")
        l > r should be(false)
      }
      "reverse" in {
        val l = datetimeperiod("[2022-05-20T10:00:00~2022-05-20T11:00:00)")
        val r = datetimeperiod("[2022-05-20T11:00:00~2022-05-20T12:00:00)")
        l > r should be(false)
      }
    }
    "isConflict" which {
      "open-close" in {
        val l = datetimeperiod("[2022-05-20T10:00:00~2022-05-20T11:00:00)")
        val r = datetimeperiod("[2022-05-20T11:00:00~2022-05-20T12:00:00)")
        l isConflict r should be(false)
      }
      "close-close" in {
        val l = datetimeperiod("[2022-05-20T10:00:00~2022-05-20T11:00:00]")
        val r = datetimeperiod("[2022-05-20T11:00:00~2022-05-20T12:00:00)")
        l isConflict r should be(true)
      }
      "same" in {
        val l = datetimeperiod("[2022-05-20T10:00:00~2022-05-20T11:00:00)")
        val r = datetimeperiod("[2022-05-20T10:00:00~2022-05-20T11:00:00)")
        l isConflict r should be(true)
      }
      "reverse" in {
        val l = datetimeperiod("[2022-05-20T11:00:00~2022-05-20T12:00:00)")
        val r = datetimeperiod("[2022-05-20T10:00:00~2022-05-20T11:00:00)")
        l < r should be(false)
      }
    }
  }
  "intersect" should {
    "intersect" which {
      "in" in {
        val l = datetimeperiod("[2022-05-20T10:00:00~2022-05-20T11:00:00)")
        val r = datetimeperiod("[2022-05-20T10:30:00~2022-05-20T10:40:00)")
        l intersect r should be(datetimeperiod("[2022-05-20T10:30:00~2022-05-20T10:40:00)"))
      }
      "left" in {
        val l = datetimeperiod("[2022-05-20T10:00:00~2022-05-20T11:00:00)")
        val r = datetimeperiod("[2022-05-20T10:30:00~2022-05-20T11:30:00)")
        l intersect r should be(datetimeperiod("[2022-05-20T10:30:00~2022-05-20T11:00:00)"))
      }
      "right" in {
        val l = datetimeperiod("[2022-05-20T10:00:00~2022-05-20T11:00:00)")
        val r = datetimeperiod("[2022-05-20T09:30:00~2022-05-20T10:30:00)")
        l intersect r should be(datetimeperiod("[2022-05-20T10:00:00~2022-05-20T10:30:00)"))
      }
      "open-close" in {
        val l = datetimeperiod("[2022-05-20T10:00:00~2022-05-20T11:00:00)")
        val r = datetimeperiod("[2022-05-20T11:00:00~2022-05-20T12:00:00)")
        l intersect r should be(DateTimePeriod.empty)
      }
      "close-close" in {
        val l = datetimeperiod("[2022-05-20T10:00:00~2022-05-20T11:00:00]")
        val r = datetimeperiod("[2022-05-20T11:00:00~2022-05-20T12:00:00)")
        l intersect r should be(datetimeperiod("[2022-05-20T11:00:00~2022-05-20T11:00:00]"))
      }
      "same" in {
        val l = datetimeperiod("[2022-05-20T10:00:00~2022-05-20T11:00:00)")
        val r = datetimeperiod("[2022-05-20T10:00:00~2022-05-20T11:00:00)")
        l intersect r should be(datetimeperiod("[2022-05-20T10:00:00~2022-05-20T11:00:00)"))
      }
      "reverse" in {
        val l = datetimeperiod("[2022-05-20T11:00:00~2022-05-20T12:00:00)")
        val r = datetimeperiod("[2022-05-20T10:00:00~2022-05-20T11:00:00)")
        l intersect r should be(DateTimePeriod.empty)
      }
    }
  }
}
