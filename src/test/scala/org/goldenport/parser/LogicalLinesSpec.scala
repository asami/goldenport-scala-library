package org.goldenport.parser

import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest._
import org.scalacheck.{Gen, Prop}

/*
 * @since   Aug. 24, 2018
 *  version Aug. 28, 2018
 *  version Oct. 25, 2018
 *  version Feb. 13, 2019
 *  version Apr. 13, 2019
 *  version Nov. 26, 2019
 *  version Jan. 20, 2020
 *  version Jan. 17, 2021
 *  version May. 11, 2021
 *  version Jan.  1, 2025
 *  version Aug. 16, 2025
 *  version Aug. 24, 2026
 * @version Nov. 18, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class LogicalLinesSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "authored source locations" should {
    "retain physical line starts across headings and blank boundaries" in {
      Given("a document with a title, blank lines and Japanese text in a table")
      val source = "Title\n=====\n\n# Body\n\n| 前xref:[#fig]後 |\n"

      When("raw logical lines retain the authored text")
      val result = LogicalLines.parse(LogicalLines.Config.raw, source)
      val nonempty = result.lines.filter(_.text.nonEmpty)

      Then("each nonempty line reports its own start instead of the preceding LF")
      nonempty.map(_.text) shouldBe Vector("Title", "=====", "# Body", "| 前xref:[#fig]後 |")
      nonempty.map(_.location.flatMap(_.line)) shouldBe Vector(Some(1), Some(2), Some(4), Some(6))
      nonempty.map(_.location.flatMap(_.offset)) shouldBe Vector.fill(4)(Some(1))
    }

    "start the first content after leading blank lines at its authored position" in {
      Given("a source beginning with two blank lines")
      val source = "\n\n本文"

      When("logical parsing reaches its first content")
      val result = LogicalLines.parse(LogicalLines.Config.raw, source)

      Then("the content retains line three and the first column")
      result.lines.find(_.text == "本文").flatMap(_.location) shouldBe Some(ParseLocation(3, 1))
    }

    "preserve positions across varied text and blank lines for LF and CRLF" in {
      Given("varied physical lines with ordinary and Japanese text and blank boundaries")
      val line = Gen.oneOf(Gen.alphaNumStr, Gen.const("日本語"), Gen.const(""))
      val cases = for {
        middle <- Gen.listOf(line)
        ending <- Gen.oneOf("\n", "\r\n")
      } yield (Vector("start") ++ middle.toVector ++ Vector("finish"), ending)
      val property = Prop.forAll(cases) { case (rows, ending) =>
        val result = LogicalLines.parse(LogicalLines.Config.raw, rows.mkString(ending))
        val expected = rows.zipWithIndex.collect {
          case (text, ordinal) if text.nonEmpty => (text, Some(ordinal + 1), Some(1))
        }
        val actual = result.lines.filter(_.text.nonEmpty).map { row =>
          (row.text, row.location.flatMap(_.line), row.location.flatMap(_.offset))
        }
        actual == expected
      }

      When("the logical parser consumes those sources")
      val outcome = org.scalacheck.Test.check(
        org.scalacheck.Test.Parameters.default.withMinSuccessfulTests(100), property
      )

      Then("all generated cases preserve text and physical source starts")
      withClue(outcome.toString) { outcome.passed shouldBe true }
    }

    "keep locations absent when location capture is disabled" in {
      Given("the same line boundaries with location capture disabled")
      val source = "first\n\nlast"

      When("the configured parser reads them")
      val result = LogicalLines.parse(LogicalLines.Config.raw.withoutLocation, source)

      Then("no logical line gains inferred location metadata")
      result.lines.forall(_.location.isEmpty) shouldBe true
    }
  }

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
    val config = LogicalLines.Config.easyText
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
      "stick" in {
        val s = "\"\"\"a\"x\"\"\"\""
        val r = parse(s)
        r should be(LogicalLines.start("\"\"\"a\"x\"\"\"\""))
      }
    }
    "back quote" which {
      "keeps nested double quoted content literal" in {
        Given("a parser configuration that enables back quotes and double quotes")
        val config = LogicalLines.Config.raw.copy(
          useBackQuote = true,
          useDoubleQuote = true
        )
        val source = """before `scala-cli "https://repo.example/repository" "org.example:library:1.0.0"` after"""

        When("LogicalLines parses a backquoted fragment containing quoted content")
        val result = LogicalLines.parse(config, source)

        Then("the complete source remains one logical line without changing the quoted fragment")
        result shouldBe LogicalLines.start(source)
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
      "back quote in text" in {
        val conf = LogicalLines.Config.easyHtml.copy(useBackQuote = true)
        val s = """<span lang="ja">`minimal.main.hello` is `<component>.<service>.<operation>`.</span>"""
        val r = LogicalLines.parse(conf, s)
        r should be(LogicalLines.start("""<span lang="ja">`minimal.main.hello` is `<component>.<service>.<operation>`.</span>"""))
      }
      "inline tag remains tag outside back quote" in {
        val conf = LogicalLines.Config.easyHtml.copy(useBackQuote = true)
        val s = """<span lang="ja">This is <i>important</i>.</span>"""
        val r = LogicalLines.parse(conf, s)
        r should be(LogicalLines.start("""<span lang="ja">This is <i>important</i>.</span>"""))
      }
      "group one multiline generic element with quoted URLs, nested tags, and a self-closing child" in {
        Given("a multiline generic element with a quoted https URL, nested markup, and an image-like child")
        val config = LogicalLines.Config.easyHtml.copy(useDoubleQuote = true, useBackQuote = true)
        val source =
          """<div href="https://example.com/path/item">
            |<span>Label with `<component>`</span>
            |<img src="images/model.png"/>
            |</div>""".stripMargin

        When("LogicalLines groups the XML-shaped source with its easy HTML configuration")
        val result = LogicalLines.parse(config, source)

        Then("one logical line retains the complete authored source without rewriting it")
        result.lines should have size 1
        result.lineVector shouldBe Vector(source)
      }
    }
  }
}
