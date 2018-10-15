package org.goldenport.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

/*
 * @since   Aug. 24, 2018
 * @version Aug. 28, 2018
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
      r should be(LogicalLines("a", "b", "c"))
    }
    "double quote" in {
      val s = """a "
b" c
"""
      val r = LogicalLines.parse(s)
      r should be(LogicalLines("""a "
b" c"""))
    }
    "s-expression" which {
      "one line" in {
        val s = """(a b c d)"""
        val r = LogicalLines.parse(s)
        r should be(LogicalLines("(a b c d)"))
      }
      "tow lines" in {
        val s = """(a b
c d)"""
        val r = LogicalLines.parse(s)
        r should be(LogicalLines("(a b\nc d)"))
      }
      "double quote" in {
        val s = """(a b "s
" c d)"""
        val r = LogicalLines.parse(s)
        r should be(LogicalLines("""(a b "s
" c d)"""))
      }
    }
    "json" which {
      "one line" in {
        val s = """{"a":"b", "c":"d"}"""
        val r = LogicalLines.parse(s)
        r should be(LogicalLines("""{"a":"b", "c":"d"}"""))
      }
      "multi lines" in {
        val s = """{
  "a":"b",
  "c":"d"
}"""
        val r = LogicalLines.parse(s)
        r should be(LogicalLines("""{
  "a":"b",
  "c":"d"
}"""))
      }
    }
    "xml" which {
      "empty tag" in {
        val s = """<a/>"""
        val r = LogicalLines.parse(s)
        r should be(LogicalLines("""<a/>"""))
      }
      "one line" in {
        val s = """<a x="10">b</a>"""
        val r = LogicalLines.parse(s)
        r should be(LogicalLines("""<a x="10">b</a>"""))
      }
      "one line nest" in {
        val s = """<a x="10"><b y="20">xyz</b></a>"""
        val r = LogicalLines.parse(s)
        r should be(LogicalLines("""<a x="10"><b y="20">xyz</b></a>"""))
      }
      "one line nest empty tag" in {
        val s = """<a x="10"><b/></a>"""
        val r = LogicalLines.parse(s)
        r should be(LogicalLines("""<a x="10"><b/></a>"""))
      }
    }
  }
}
