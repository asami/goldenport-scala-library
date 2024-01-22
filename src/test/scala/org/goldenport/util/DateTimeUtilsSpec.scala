package org.goldenport.util

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import org.joda.time._
import org.joda.time.format.{ISODateTimeFormat, DateTimeFormat}

/*
 * @since   Jun. 17, 2022
 * @version Jun. 17, 2022
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class DateTimeUtilsSpec extends WordSpec with Matchers with GivenWhenThen {
  "parseIsoDateTime" should {
    "parse" which {
      "2022-06-20T18:00:00+09:00" in {
        val tz = DateTimeUtils.jodagmt
        // println(ISODateTimeFormat.dateTimeParser.withOffsetParsed().withZone(tz).parseDateTime("2022-06-20T18:00:00+09:00"))
        // println(ISODateTimeFormat.dateTimeParser.withOffsetParsed().parseDateTime("2022-06-20T18:00:00+09:00"))
        // println(ISODateTimeFormat.dateTimeParser.withOffsetParsed().withZone(DateTimeUtils.jodajst).parseDateTime("2022-06-20T18:00:00+09:00"))
        // println(ISODateTimeFormat.dateTimeParser.withOffsetParsed().parseDateTime("2022-06-20T18:00:00"))
        DateTimeUtils.parseIsoDateTime("2022-06-20T18:00:00+09:00", tz).toString should be("2022-06-20T09:00:00.000Z")
      }
      "2022-06-20T18:00:00" in {
        val tz = DateTimeUtils.jodajst
        DateTimeUtils.parseIsoDateTime("2022-06-20T18:00:00", tz).toString should be("2022-06-20T18:00:00.000+09:00")
      }
    }
  }
  "parseDateTime" should {
    "parse" which {
      "2022-06-20T18:00:00 GMT" in {
        val tz = DateTimeUtils.jodagmt
        DateTimeUtils.parseDateTime("2022-06-20T18:00:00", tz).toString should be("2022-06-20T18:00:00.000Z")
      }
      "2022-06-20T18:00:00 JST" in {
        val tz = DateTimeUtils.jodajst
        DateTimeUtils.parseDateTime("2022-06-20T18:00:00", tz).toString should be("2022-06-20T18:00:00.000+09:00")
      }
      "2022-06-20T18:00:00+09:00" in {
        val tz = DateTimeUtils.jodagmt
        DateTimeUtils.parseDateTime("2022-06-20T18:00:00+09:00", tz).toString should be("2022-06-20T18:00:00.000+09:00")
      }
      "2022-06-20T18:00:00+JST" in {
        val tz = DateTimeUtils.jodagmt
        DateTimeUtils.parseDateTime("2022-06-20T18:00:00+09:00", tz).toString should be("2022-06-20T18:00:00.000+09:00")
      }
      "2022-06-20T18:00:00-09:00" in {
        val tz = DateTimeUtils.jodagmt
        DateTimeUtils.parseDateTime("2022-06-20T18:00:00-09:00", tz).toString should be("2022-06-20T18:00:00.000-09:00")
      }

    }
  }
}
