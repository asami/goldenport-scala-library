package org.goldenport.util

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import org.joda.time.LocalDateTime

/*
 * @since   Jan. 27, 2022
 * @version Jan. 27, 2022
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class LocalDateTimeUtilsSpec extends WordSpec with Matchers with GivenWhenThen {
  import SeqUtils._

  "LocalDateTimeUtils" should {
    "countOfMonthsAlmost" which {
      "same month, later day" in {
        val s = new LocalDateTime(2022, 1, 27, 0, 0)
        val e = new LocalDateTime(2022, 1, 29, 0, 0)
        LocalDateTimeUtils.countOfMonthsAlmost(s, e) should be(1)
      }
      "next month, previous day" in {
        val s = new LocalDateTime(2022, 1, 27, 0, 0)
        val e = new LocalDateTime(2022, 2, 26, 0, 0)
        LocalDateTimeUtils.countOfMonthsAlmost(s, e) should be(1)
      }
      "next month, same day" in {
        val s = new LocalDateTime(2022, 1, 27, 0, 0)
        val e = new LocalDateTime(2022, 2, 27, 0, 0)
        LocalDateTimeUtils.countOfMonthsAlmost(s, e) should be(2)
      }
      "two months, early day" in {
        val s = new LocalDateTime(2022, 1, 27, 0, 0)
        val e = new LocalDateTime(2022, 3, 10, 0, 0)
        LocalDateTimeUtils.countOfMonthsAlmost(s, e) should be(2)
      }
      "tow months, later day" in {
        val s = new LocalDateTime(2022, 1, 27, 0, 0)
        val e = new LocalDateTime(2022, 3, 29, 0, 0)
        LocalDateTimeUtils.countOfMonthsAlmost(s, e) should be(3)
      }
    }
    "countOfMonthsPassed" which {
      "same month, later day" in {
        val s = new LocalDateTime(2022, 1, 27, 0, 0)
        val e = new LocalDateTime(2022, 1, 29, 0, 0)
        LocalDateTimeUtils.countOfMonthsPassed(s, e) should be(0)
      }
      "next month, same day" in {
        val s = new LocalDateTime(2022, 1, 27, 0, 0)
        val e = new LocalDateTime(2022, 2, 27, 0, 0)
        LocalDateTimeUtils.countOfMonthsPassed(s, e) should be(1)
      }
      "two months, early day" in {
        val s = new LocalDateTime(2022, 1, 27, 0, 0)
        val e = new LocalDateTime(2022, 3, 10, 0, 0)
        LocalDateTimeUtils.countOfMonthsPassed(s, e) should be(1)
      }
      "tow months, later day" in {
        val s = new LocalDateTime(2022, 1, 27, 0, 0)
        val e = new LocalDateTime(2022, 3, 29, 0, 0)
        LocalDateTimeUtils.countOfMonthsPassed(s, e) should be(2)
      }
    }
  }
}
