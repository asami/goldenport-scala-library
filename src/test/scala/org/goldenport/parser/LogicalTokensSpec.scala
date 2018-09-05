package org.goldenport.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import scala.xml._
import play.api.libs.json._

/*
 * @since   Aug. 24, 2018
 * @version Sep.  2, 2018
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class LogicalTokensSpec extends WordSpec with Matchers with GivenWhenThen {
  "LogicalTokens" should {
    "normal tokens" in {
      val s = """a
b
c
"""
      val r = LogicalTokens.parse(s)
      r should be(LogicalTokens(
        AtomToken("a", ParseLocation.init),
        SpaceToken("\n", ParseLocation(1, 2)),
        AtomToken("b", ParseLocation(2, 1)),
        SpaceToken("\n", ParseLocation(2, 2)),
        AtomToken("c", ParseLocation(3, 1)),
        SpaceToken("\n", ParseLocation(3, 2))
      ))
    }
    "double quote" in {
      val s = """a "
b" c
"""
      val r = LogicalTokens.parse(s)
      r should be(LogicalTokens(
        AtomToken("a", ParseLocation.init),
        SpaceToken(" ", ParseLocation(1, 2)),
        DoubleStringToken("\nb", ParseLocation(1, 3)),
        SpaceToken(" ", ParseLocation(2, 3)),
        AtomToken("c", ParseLocation(2, 4)),
        SpaceToken("\n", ParseLocation(2, 5))
      ))
    }
    "s-expression" which {
      "one line" in {
        val s = """(a b c d)"""
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(
          DelimiterToken("(", ParseLocation.init),
          AtomToken("a", ParseLocation(1, 2)),
          SpaceToken(" ", ParseLocation(1, 3)),
          AtomToken("b", ParseLocation(1, 4)),
          SpaceToken(" ", ParseLocation(1, 5)),
          AtomToken("c", ParseLocation(1, 6)),
          SpaceToken(" ", ParseLocation(1, 7)),
          AtomToken("d", ParseLocation(1, 8)),
          DelimiterToken(")", ParseLocation(1, 9))
        ))
      }
      "two tokens" in {
        val s = """(a b
c d)"""
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(
          DelimiterToken("(", ParseLocation.init),
          AtomToken("a", ParseLocation(1, 2)),
          SpaceToken(" ", ParseLocation(1, 3)),
          AtomToken("b", ParseLocation(1, 4)),
          SpaceToken("\n", ParseLocation(1, 5)),
          AtomToken("c", ParseLocation(2, 1)),
          SpaceToken(" ", ParseLocation(2, 2)),
          AtomToken("d", ParseLocation(2, 3)),
          DelimiterToken(")", ParseLocation(2, 4))
        ))
      }
      "double quote" in {
        val s = """(a b "s
" c d)"""
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(
          DelimiterToken("(", ParseLocation.init),
          AtomToken("a", ParseLocation(1, 2)),
          SpaceToken(" ", ParseLocation(1, 3)),
          AtomToken("b", ParseLocation(1, 4)),
          SpaceToken(" ", ParseLocation(1, 5)),
          DoubleStringToken("s\n", ParseLocation(1, 6)),
          SpaceToken(" ", ParseLocation(2, 2)),
          AtomToken("c", ParseLocation(2, 3)),
          SpaceToken(" ", ParseLocation(2, 4)),
          AtomToken("d", ParseLocation(2, 5)),
          DelimiterToken(")", ParseLocation(2, 6))
        ))
      }
    }
    "json" which {
      "one line" in {
        val s = """{"a":"b", "c":"d"}"""
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(
          JsonParser.JsonToken(Json.parse(s), ParseLocation.init)
        ))
      }
      "multi lines" in {
        val s = """{
  "a":"b",
  "c":"d"
}"""
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(
          JsonParser.JsonToken(Json.parse(s), ParseLocation.init)
        ))
      }
    }
    "xml" which {
      "empty tag" in {
        val s = """<a/>"""
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(XmlParser.XmlToken(XML.loadString(s), ParseLocation.init)))
      }
      "one line" in {
        val s = """<a x="10">b</a>"""
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(XmlParser.XmlToken(XML.loadString(s), ParseLocation.init)))
      }
      "one line nest" in {
        val s = """<a x="10"><b y="20">xyz</b></a>"""
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(XmlParser.XmlToken(XML.loadString(s), ParseLocation.init)))
      }
      "one line nest empty tag" in {
        val s = """<a x="10"><b/></a>"""
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(XmlParser.XmlToken(XML.loadString(s), ParseLocation.init)))
      }
   }
  }
}
