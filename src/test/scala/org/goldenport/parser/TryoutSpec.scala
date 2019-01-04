package org.goldenport.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import org.joda.time._
import org.goldenport.parser._

/*
 * @since   Jan.  1, 2019
 * @version Jan.  3, 2019
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class TryoutSpec extends WordSpec with Matchers with GivenWhenThen {
  "token" should {
    // "number" which {
    //   "localdate" in {
    //     val s = "2018-09-09"
    //     val r = LogicalTokens.parse(s)
    //     r should be(LogicalTokens(
    //       LocalDateToken(LocalDate.parse(s), ParseLocation.init)
    //     ))
    //   }
    // }
    // "jxpath" which {
    //   "typical" in {
    //     val s = """/a/b/c[id='z']"""
    //     val r = LogicalTokens.parse(s)
    //     r should be(LogicalTokens(
    //       PathToken(s, ParseLocation.init)
    //     ))
    //   }
    // }
    "double quote in raw string" in {
      val s = "\"\"\"\"\"\"\""
      val r = LogicalTokens.parseDebug(s)
      r should be(LogicalTokens(
        RawStringToken("\"", ParseLocation.init)
      ))
    }
    "double quote in raw string 2" in {
      val s = "\"\"\"\"\"\"\"\""
      val r = LogicalTokens.parseDebug(s)
      r should be(LogicalTokens(
        RawStringToken("\"\"", ParseLocation.init)
      ))
    }
  }
}
