package org.goldenport.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

/*
 * @since   Aug. 24, 2018
 *  version Aug. 28, 2018
 *  version Oct. 25, 2018
 *  version Feb. 13, 2019
 *  version Apr. 13, 2019
 *  version Nov. 26, 2019
 *  version Jan. 20, 2020
 * @version Jan. 17, 2021
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class LogicalLinesSpec extends WordSpec with Matchers with GivenWhenThen {
  "raw" should {
    val config = LogicalLines.Config.raw
    def parse(p: String) = LogicalLines.parse(config, p)

    "one" in {
      val s = """'"""
      val r = parse(s)
      r should be(LogicalLines("""'""", ParseLocation.start))
    }
    "after one" in {
      val s = """a'"""
      val r = parse(s)
      r should be(LogicalLines.start("""a'"""))
    }
    "in one" in {
      val s = """a'b"""
      val r = parse(s)
      r should be(LogicalLines.start("""a'b"""))
    }
  }

  "easytext" should {
    val config = LogicalLines.Config.easytext
  }

  "script" should {
    val config = LogicalLines.Config.script
    def parse(p: String) = LogicalLines.parse(config, p)

    "normal lines" in {
      val s = """a
b
c
"""
      val r = parse(s)
      r should be(LogicalLines.start("a", "b", "c"))
    }
    "one syntax error" in {
      val s = """'"""
      an [ParseSyntaxErrorException] should be thrownBy parse(s)
    }
    "double quote" which {
      "new line" in {
      val s = """a "
b" c
"""
      val r = parse(s)
      r should be(LogicalLines.start("""a "
b" c"""))
      }
      "one" in {
        val s = "\"\"\"a \"x \"\"\""
        val r = parse(s)
        r should be(LogicalLines.start("\"\"\"a \"x \"\"\""))
      }
    }
    "lisp" which {
      val conf = LogicalLines.Config.lisp
      def parselisp(p: String) = LogicalLines.parse(conf, p)

      "single quote" in {
        val s = """'a"""
        val r = parselisp(s)
        r should be(LogicalLines.start("""'a"""))
      }
    }
    "s-expression" which {
      "one line" in {
        val s = """(a b c d)"""
        val r = parse(s)
        r should be(LogicalLines.start("(a b c d)"))
      }
      "tow lines" in {
        val s = """(a b
c d)"""
        val r = parse(s)
        r should be(LogicalLines.start("(a b\nc d)"))
      }
      "double quote" in {
        val s = """(a b "s
" c d)"""
        val r = parse(s)
        r should be(LogicalLines.start("""(a b "s
" c d)"""))
      }
    }
    "json" which {
      "one line" in {
        val s = """{"a":"b", "c":"d"}"""
        val r = parse(s)
        r should be(LogicalLines.start("""{"a":"b", "c":"d"}"""))
      }
      "multi lines" in {
        val s = """{
  "a":"b",
  "c":"d"
}"""
        val r = parse(s)
        r should be(LogicalLines.start("""{
  "a":"b",
  "c":"d"
}"""))
      }
      "number value" in {
        val s = """{"a":1}"""
        val r = parse(s)
        r should be(LogicalLines.start("""{"a":1}"""))
      }
    }
    "xml" which {
      "empty tag" in {
        val s = """<a/>"""
        val r = parse(s)
        r should be(LogicalLines.start("""<a/>"""))
      }
      "one line" in {
        val s = """<a x="10">b</a>"""
        val r = parse(s)
        r should be(LogicalLines.start("""<a x="10">b</a>"""))
      }
      "one line nest" in {
        val s = """<a x="10"><b y="20">xyz</b></a>"""
        val r = parse(s)
        r should be(LogicalLines.start("""<a x="10"><b y="20">xyz</b></a>"""))
      }
      "one line nest empty tag" in {
        val s = """<a x="10"><b/></a>"""
        val r = parse(s)
        r should be(LogicalLines.start("""<a x="10"><b/></a>"""))
      }
    }
  }
}
