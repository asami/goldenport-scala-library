package org.goldenport.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import org.joda.time._
import org.goldenport.parser._

/*
 * @since   Jan.  1, 2019
 *  version Feb.  9, 2019
 *  version Mar. 10, 2019
 * @version Apr. 13, 2019
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class TryoutSpec extends WordSpec with Matchers with GivenWhenThen {
  "token" should {
    // "json" which {
    //   "number value" in {
    //     val s = """{"a":1}"""
    //     val r = LogicalTokens.parse(s)
    //     r should be(LogicalTokens(
    //       JsonParser.JsonToken(s, ParseLocation.start)
    //     ))
    //   }
    // }
  }
}
