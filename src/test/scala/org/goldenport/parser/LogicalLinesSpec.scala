package org.goldenport.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

/*
 * @since   Aug. 24, 2018
 *  version Aug. 28, 2018
 *  version Oct. 25, 2018
 * @version Feb. 13, 2019
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class LogicalLinesSpec extends WordSpec with Matchers with GivenWhenThen {
  "LogicalLines" should {
    "normal lines" in {
      val s = """a
b
c
"""
      val r = LogicalLines.parse(s)
      r should be(LogicalLines.start("a", "b", "c"))
    }
    "double quote" in {
      val s = """a "
b" c
"""
      val r = LogicalLines.parse(s)
      r should be(LogicalLines.start("""a "
b" c"""))
    }
    "single quote" which {
      "one" in {
        val s = """'"""
        val conf = LogicalLines.Config.raw
        val r = LogicalLines.parse(conf, s)
        r should be(LogicalLines("""'""", ParseLocation.start))
      }
      "one syntax error" in {
        val s = """'"""
        an [ParseSyntaxErrorException] should be thrownBy LogicalLines.parse(s)
      }
      "after one" in {
        val s = """a'"""
        val conf = LogicalLines.Config.raw
        val r = LogicalLines.parse(conf, s)
        r should be(LogicalLines.start("""a'"""))
      }
      "in one" in {
        val s = """a'b"""
        val conf = LogicalLines.Config.raw
        val r = LogicalLines.parse(conf, s)
        r should be(LogicalLines.start("""a'b"""))
      }
      "lisp" in {
        val s = """'a"""
        val conf = LogicalLines.Config.lisp
        val r = LogicalLines.parse(conf, s)
        r should be(LogicalLines.start("""'a"""))
      }
    }
    "s-expression" which {
      "one line" in {
        val s = """(a b c d)"""
        val r = LogicalLines.parse(s)
        r should be(LogicalLines.start("(a b c d)"))
      }
      "tow lines" in {
        val s = """(a b
c d)"""
        val r = LogicalLines.parse(s)
        r should be(LogicalLines.start("(a b\nc d)"))
      }
      "double quote" in {
        val s = """(a b "s
" c d)"""
        val r = LogicalLines.parse(s)
        r should be(LogicalLines.start("""(a b "s
" c d)"""))
      }
    }
    "json" which {
      "one line" in {
        val s = """{"a":"b", "c":"d"}"""
        val r = LogicalLines.parse(s)
        r should be(LogicalLines.start("""{"a":"b", "c":"d"}"""))
      }
      "multi lines" in {
        val s = """{
  "a":"b",
  "c":"d"
}"""
        val r = LogicalLines.parse(s)
        r should be(LogicalLines.start("""{
  "a":"b",
  "c":"d"
}"""))
      }
    }
    "xml" which {
      "empty tag" in {
        val s = """<a/>"""
        val r = LogicalLines.parse(s)
        r should be(LogicalLines.start("""<a/>"""))
      }
      "one line" in {
        val s = """<a x="10">b</a>"""
        val r = LogicalLines.parse(s)
        r should be(LogicalLines.start("""<a x="10">b</a>"""))
      }
      "one line nest" in {
        val s = """<a x="10"><b y="20">xyz</b></a>"""
        val r = LogicalLines.parse(s)
        r should be(LogicalLines.start("""<a x="10"><b y="20">xyz</b></a>"""))
      }
      "one line nest empty tag" in {
        val s = """<a x="10"><b/></a>"""
        val r = LogicalLines.parse(s)
        r should be(LogicalLines.start("""<a x="10"><b/></a>"""))
      }
    }
  }
}
