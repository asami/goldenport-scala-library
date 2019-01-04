package org.goldenport.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import org.joda.time._
import scala.xml._
import play.api.libs.json._
import org.goldenport.util.DateTimeUtils

/*
 * @since   Aug. 24, 2018
 *  version Sep. 22, 2018
 * @version Jan.  1, 2019
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class LogicalTokensSpec extends WordSpec with Matchers with GivenWhenThen {
  "token" should {
    "number" which {
      "integer" in {
        val s = "1"
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(
          NumberToken(1, ParseLocation.init)
        ))
      }
      "url" in {
        val s = "http://www.yahoo.com"
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(
          UrlToken("http://www.yahoo.com", ParseLocation.init)
        ))
      }
      "urn" in {
        val s = "URN:ISBN:4-8399-0454-5"
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(
          UrnToken("URN:ISBN:4-8399-0454-5", ParseLocation.init)
        ))
      }
      "datetime" in {
        val s = "2018-09-09T10:11:12+09:00"
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(
          DateTimeToken(DateTimeUtils.parseIsoDateTimeJst(s), ParseLocation.init)
        ))
      }
      "localdate" in {
        val s = "2018-09-09"
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(
          LocalDateToken(LocalDate.parse(s), ParseLocation.init)
        ))
      }
      "localtime" in {
        val s = "10:11:12"
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(
          LocalTimeToken(LocalTime.parse(s), ParseLocation.init)
        ))
      }
    }
    "string" which {
      "typical" in {
        val s = "\"abc\""
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(
          DoubleStringToken("abc", ParseLocation.init)
        ))
      }
      "multi line" in {
        val s = "\"abc\nxyz\""
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(
          DoubleStringToken("abc\nxyz", ParseLocation.init)
        ))
      }
    }
    "single string" which {
      "typical" in {
        val s = "'abc'"
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(
          SingleStringToken("abc", ParseLocation.init)
        ))
      }
      "multi line" in {
        val s = """'abc
xyz'"""
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(
          SingleStringToken("abc\nxyz", ParseLocation.init)
        ))
      }
    }
    "raw string" which {
      "typical" in {
        val s = "\"\"\"abc\"\"\""
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(
          RawStringToken("abc", ParseLocation.init)
        ))
      }
      "multi line" in {
        val s = "\"\"\"abc\nxyz\"\"\""
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(
          RawStringToken("abc\nxyz", ParseLocation.init)
        ))
      }
    }
    "json" which {
      "one line" in {
        val s = """{"a":"b", "c":"d"}"""
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(
          JsonParser.JsonToken(s, ParseLocation.init)
        ))
      }
      "multi lines" in {
        val s = """{
  "a":"b",
  "c":"d"
}"""
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(
          JsonParser.JsonToken(s, ParseLocation.init)
        ))
      }
    }
    "xml" which {
      "empty tag" in {
        val s = """<a/>"""
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(
          XmlParser.XmlToken(s, ParseLocation.init)
        ))
      }
      "one line" in {
        val s = """<a x="10">b</a>"""
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(
          XmlParser.XmlToken(s, ParseLocation.init)
        ))
      }
      "one line nest" in {
        val s = """<a x="10"><b y="20">xyz</b></a>"""
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(
          XmlParser.XmlToken(s, ParseLocation.init)
        ))
      }
      "one line nest empty tag" in {
        val s = """<a x="10"><b/></a>"""
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(
          XmlParser.XmlToken(s, ParseLocation.init)
        ))
      }
    }
    "jxpath" which {
      "typical" in {
        val s = """/a/b/c"""
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(
          PathToken(s, ParseLocation.init)
        ))
      }
    }
    "jexl" which {
    }
  }
  "line" should {
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
      "numbers" in {
        val s = """(1 2 30 456)"""
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(
          DelimiterToken("(", ParseLocation.init),
          NumberToken(1, ParseLocation(1, 2)),
          SpaceToken(" ", ParseLocation(1, 3)),
          NumberToken(2, ParseLocation(1, 4)),
          SpaceToken(" ", ParseLocation(1, 5)),
          NumberToken(30, ParseLocation(1, 6)),
          SpaceToken(" ", ParseLocation(1, 8)),
          NumberToken(456, ParseLocation(1, 9)),
          DelimiterToken(")", ParseLocation(1, 12))
        ))
      }
      "two line" in {
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
  }
}
