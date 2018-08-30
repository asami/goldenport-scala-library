package org.goldenport.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import play.api.libs.json._

/*
 * @since   Aug. 24, 2018
 * @version Aug. 29, 2018
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
        AtomToken("a"),
        SpaceToken("\n"),
        AtomToken("b"),
        SpaceToken("\n"),
        AtomToken("c"),
        SpaceToken("\n")
      ))
    }
    "double quote" in {
      val s = """a "
b" c
"""
      val r = LogicalTokens.parse(s)
      r should be(LogicalTokens(
        AtomToken("a"),
        SpaceToken(" "),
        DoubleStringToken("\nb"),
        SpaceToken(" "),
        AtomToken("c"),
        SpaceToken("\n")
      ))
    }
    "s-expression" which {
      "one line" in {
        val s = """(a b c d)"""
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(Vector(
          DelimiterToken("("),
          AtomToken("a"),
          SpaceToken(" "),
          AtomToken("b"),
          SpaceToken(" "),
          AtomToken("c"),
          SpaceToken(" "),
          AtomToken("d"),
          DelimiterToken(")")
        )))
      }
      "two tokens" in {
        val s = """(a b
c d)"""
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(Vector(
          DelimiterToken("("),
          AtomToken("a"),
          SpaceToken(" "),
          AtomToken("b"),
          SpaceToken("\n"),
          AtomToken("c"),
          SpaceToken(" "),
          AtomToken("d"),
          DelimiterToken(")")
        )))
      }
      "double quote" in {
        val s = """(a b "s
" c d)"""
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(Vector(
          DelimiterToken("("),
          AtomToken("a"),
          SpaceToken(" "),
          AtomToken("b"),
          SpaceToken(" "),
          DoubleStringToken("s\n"),
          SpaceToken(" "),
          AtomToken("c"),
          SpaceToken(" "),
          AtomToken("d"),
          DelimiterToken(")")
        )))
      }
    }
    "json" which {
      "one line" in {
        val s = """{"a":"b", "c":"d"}"""
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(
          JsonParser.JsonToken(Json.parse(s))
        ))
      }
      "multi lines" in {
        val s = """{
  "a":"b",
  "c":"d"
}"""
        val r = LogicalTokens.parse(s)
        r should be(LogicalTokens(
          JsonParser.JsonToken(Json.parse(s))
        ))
      }
    }
//     "xml" which {
//       "empty tag" in {
//         val s = """<a/>"""
//         val r = LogicalTokens.parse(s)
//         // r should be(LogicalTokens(Vector("""<a/>""")))
//       }
//       "one line" in {
//         val s = """<a x="10">b</a>"""
//         val r = LogicalTokens.parse(s)
//         // r should be(LogicalTokens(Vector("""<a x="10">b</a>""")))
//       }
//       "one line nest" in {
//         val s = """<a x="10"><b y="20">xyz</b></a>"""
//         val r = LogicalTokens.parse(s)
//         // r should be(LogicalTokens(Vector("""<a x="10"><b y="20">xyz</b></a>""")))
//       }
//       "one line nest empty tag" in {
//         val s = """<a x="10"><b/></a>"""
//         val r = LogicalTokens.parse(s)
//         // r should be(LogicalTokens(Vector("""<a x="10"><b/></a>""")))
//       }
//    }
  }
}
