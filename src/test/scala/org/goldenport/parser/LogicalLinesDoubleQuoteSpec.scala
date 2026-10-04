package org.goldenport.parser

import com.typesafe.config.{ConfigFactory, ConfigParseOptions, ConfigRenderOptions, ConfigSyntax, ConfigValueFactory}
import org.scalacheck.Gen
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/*
 * @since   Oct.  4, 2026
 * @version Oct.  4, 2026
 */
class LogicalLinesDoubleQuoteSpec extends AnyWordSpec with Matchers with GivenWhenThen
    with ScalaCheckPropertyChecks {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  private val _slash_count = Gen.choose(0, 12)
  private val _display_string = for {
    count <- Gen.choose(0, 8)
    fragments <- Gen.listOfN(count, Gen.oneOf(
      "日本語", "é", "😀", "\"", "\\", "\n", "\t", "\r", "\b", "\f", "\u0001", "  "
    ))
    slashcount <- Gen.choose(0, 5)
  } yield "  題名 \" " + ("\\" * slashcount) + fragments.mkString + " 終端  "

  private def _encoded_literal(text: String): String =
    ConfigValueFactory.fromAnyRef(text).render(ConfigRenderOptions.concise().setJson(true))

  private def _decode_literal(literal: String): String =
    ConfigFactory.parseString(
      "{\"value\":" + literal + "}",
      ConfigParseOptions.defaults().setSyntax(ConfigSyntax.JSON)
    ).getString("value")

  private def _following_lines(line: String): Vector[String] =
    Vector(line, "ordinary = next", "label = \"後続\"")

  private def _document(literal: String): String = Vector(
    "# LOGICAL-UI",
    "title = " + literal,
    "producerId = \"fixture\"",
    "## LIST",
    "title = " + literal,
    "screenId = \"list\"",
    "## DETAIL",
    "title = \"詳細\"",
    "screenId = \"detail\""
  ).mkString("\n")

  "LogicalLines double-quoted literals" should {
    "honor the parity of consecutive backslashes" which {
      Vector(1, 3, 5).foreach { slashcount =>
        s"retain a single embedded quote after $slashcount backslashes without absorbing following lines" in {
          Given("a closed literal with one escaped embedded quote followed by ordinary and quoted properties")
          val literal = "\"left" + ("\\" * slashcount) + "\"right\""
          val expected = _following_lines("title = " + literal)
          val source = expected.mkString("\n")

          When("the script configuration groups the physical lines")
          val result = LogicalLines.parse(LogicalLines.Config.script, source)
          val decoded = _decode_literal(result.lineVector.head.stripPrefix("title = "))

          Then("the quote and every backslash remain lexical content and following lines remain separate")
          result.lineVector shouldBe expected
          decoded shouldBe "left" + ("\\" * (slashcount / 2)) + "\"right"
        }
      }
      Vector(0, 2, 4).foreach { slashcount =>
        s"terminate a literal after $slashcount backslashes before following properties" in {
          Given("a closing quote preceded by an even backslash run and two subsequent physical lines")
          val literal = "\"left" + ("\\" * slashcount) + "\""
          val expected = _following_lines("title = " + literal)
          val source = expected.mkString("\n")

          When("the script configuration groups and JSON interprets the retained literal")
          val result = LogicalLines.parse(LogicalLines.Config.script, source)
          val decoded = _decode_literal(result.lineVector.head.stripPrefix("title = "))

          Then("the closing quote terminates the literal while its lexical backslashes are preserved")
          result.lineVector shouldBe expected
          decoded shouldBe "left" + ("\\" * (slashcount / 2))
        }
      }
    }

    "preserve authored literal content" which {
      "retain repeated embedded quotes and the following property boundary" in {
        Given("a valid encoded literal containing several embedded quotes")
        val display = "one \"two\" three \"four"
        val expected = _following_lines("title = " + _encoded_literal(display))
        val source = expected.mkString("\n")

        When("the script configuration groups the properties")
        val result = LogicalLines.parse(LogicalLines.Config.script, source)
        val decoded = _decode_literal(result.lineVector.head.stripPrefix("title = "))

        Then("all quote characters survive grouping and JSON decoding recovers the authored value")
        result.lineVector shouldBe expected
        decoded shouldBe display
      }

      Vector("", "  日本語の題名  ", "  café 😀 e\u0301 \"表示  ").foreach { display =>
        s"retain the encoded bytes of ${_encoded_literal(display)}" in {
          Given("an empty or nonblank padded Unicode literal followed by separate properties")
          val expected = _following_lines("title = " + _encoded_literal(display))
          val source = expected.mkString("\n")

          When("the script configuration groups the properties and JSON decodes the retained literal")
          val result = LogicalLines.parse(LogicalLines.Config.script, source)
          val decoded = _decode_literal(result.lineVector.head.stripPrefix("title = "))

          Then("grouping neither trims nor normalizes the encoded literal or decoded display value")
          result.lineVector shouldBe expected
          decoded shouldBe display
        }
      }

      "retain lexical control and Unicode escapes for downstream JSON decoding" in {
        Given("a valid literal spelling newline, tab, return, backspace, form-feed, Unicode, quote and slash escapes")
        val literal = "\"line\\n\\t\\r\\b\\f\\u65e5\\\"\\\\end\""
        val expected = _following_lines("title = " + literal)
        val source = expected.mkString("\n")

        When("the script configuration groups the properties and JSON decodes the retained literal")
        val result = LogicalLines.parse(LogicalLines.Config.script, source)
        val decoded = _decode_literal(result.lineVector.head.stripPrefix("title = "))

        Then("the logical line preserves escape spelling while JSON owns its decoded meaning")
        result.lineVector shouldBe expected
        decoded shouldBe "line\n\t\r\b\f日\"\\end"
      }
    }

    "retain configuration and grouping compatibility" which {
      Vector("script" -> LogicalLines.Config.script, "lisp" -> LogicalLines.Config.lisp).foreach {
        case (label, conf) =>
          s"preserve escaped quotes and following lines with the $label configuration" in {
            Given("a quote-enabled configuration and one escaped quote in a closed property literal")
            val expected = _following_lines("title = " + _encoded_literal("題名 \"表示"))
            val source = expected.mkString("\n")

            When("that configuration groups the physical properties")
            val result = LogicalLines.parse(conf, source)

            Then("the literal stays unchanged and both subsequent properties keep their boundaries")
            result.lineVector shouldBe expected
          }
      }

      "keep physical line boundaries when the raw configuration disables quoting" in {
        Given("quote-disabled raw configuration and quote-shaped text across physical lines")
        val expected = Vector("title = \"first\\\"", "second\"", "label = \"後続\"")
        val source = expected.mkString("\n")

        When("the raw configuration groups those physical lines")
        val result = LogicalLines.parse(LogicalLines.Config.raw, source)

        Then("quotes and backslashes are ordinary characters and do not group the lines")
        result.lineVector shouldBe expected
      }

      "keep valid multiline quoted content in one logical line" in {
        Given("a closed multiline literal with an escaped quote on its first physical line")
        val expected = _following_lines("title = \"first\\\"\nsecond\"")
        val source = expected.mkString("\n")

        When("the script configuration groups the multiline literal")
        val result = LogicalLines.parse(LogicalLines.Config.script, source)

        Then("the literal retains its physical newline and later properties remain separate")
        result.lineVector shouldBe expected
      }

      "keep nested JSON content together without swallowing the following line" in {
        Given("a multiline nested JSON object with an escaped quote in a string value")
        val json = "{\n  \"outer\": {\"title\": " + _encoded_literal("日本語 \"表示") + "}\n}"
        val expected = _following_lines(json)
        val source = expected.mkString("\n")

        When("the script configuration groups the nested JSON object")
        val result = LogicalLines.parse(LogicalLines.Config.script, source)
        val decoded = ConfigFactory.parseString(
          result.lineVector.head, ConfigParseOptions.defaults().setSyntax(ConfigSyntax.JSON)
        ).getString("outer.title")

        Then("the complete object remains lexical content and its string value decodes without loss")
        result.lineVector shouldBe expected
        decoded shouldBe "日本語 \"表示"
      }

      "keep nested S-expression content together without swallowing the following line" in {
        Given("a multiline nested S-expression containing an escaped quote in a string")
        val expression = "(outer (title " + _encoded_literal("日本語 \"表示") + ")\n  (label \"後続\"))"
        val expected = _following_lines(expression)
        val source = expected.mkString("\n")

        When("the lisp configuration groups the nested S-expression")
        val result = LogicalLines.parse(LogicalLines.Config.lisp, source)

        Then("nested parentheses and the literal remain one unchanged logical line")
        result.lineVector shouldBe expected
      }

      "retain triple-quoted raw termination even after a backslash" in {
        Given("a multiline raw triple-quoted literal ending with a backslash before its triple delimiter")
        val raw = "title = \"\"\"first \"quoted\"\nlast" + "\\" + "\"\"\""
        val expected = _following_lines(raw)
        val source = expected.mkString("\n")

        When("the script configuration groups the raw literal")
        val result = LogicalLines.parse(LogicalLines.Config.script, source)

        Then("the existing triple delimiter closes the raw literal and following lines stay separate")
        result.lineVector shouldBe expected
      }
    }

    "preserve ordinary forLisp document section ownership" which {
      Vector(
        "with locations" -> LogicalBlocks.Config.default.forLisp,
        "without locations" -> LogicalBlocks.Config.noLocation.forLisp
      ).foreach { case (label, conf) =>
        s"keep ordered LIST and DETAIL sibling sections and their prologues $label" in {
          Given("ordinary document properties with an escaped quote before LIST and DETAIL siblings")
          val literal = _encoded_literal("  日本語 \"表示  ")
          val source = _document(literal)

          When("the ordinary forLisp document configuration parses the section tree")
          val result = LogicalBlocks.parse(conf, source)

          Then("the root and sibling section order and direct prologue line vectors survive quoting")
          result.sections.map(_.nameForModel) shouldBe Vector("LOGICAL-UI")
          val root = result.sections.head
          root.prologue.lines.lineVector shouldBe Vector("title = " + literal, "producerId = \"fixture\"")
          root.sections.map(_.nameForModel) shouldBe Vector("LIST", "DETAIL")
          root.sections.map(_.prologue.lines.lineVector) shouldBe Vector(
            Vector("title = " + literal, "screenId = \"list\""),
            Vector("title = \"詳細\"", "screenId = \"detail\"")
          )
          root.sections.map(_.sections) shouldBe Vector(Vector.empty, Vector.empty)

          And("section and property locations follow the selected configuration")
          val sectionlocations = Vector(root.location) ++ root.sections.map(_.location)
          val propertylocations = root.prologue.lines.lines.map(_.location) ++
            root.sections.flatMap(_.prologue.lines.lines.map(_.location))
          sectionlocations shouldBe (if (conf.isLocation)
            Vector(1, 4, 7).map(row => Some(ParseLocation(row, 1)))
          else Vector.fill(3)(None))
          propertylocations shouldBe (if (conf.isLocation)
            Vector(2, 3, 5, 6, 8, 9).map(row => Some(ParseLocation(row, 1)))
          else Vector.fill(6)(None))
        }
      }
    }

    "preserve generated valid literals as properties" which {
      "respect varying odd and even backslash runs for at least 100 cases" in {
        forAll(_slash_count) { slashcount =>
          Given("a valid encoded literal whose quote is embedded for odd slash runs and terminal for even runs")
          val embedded = slashcount % 2 == 1
          val literal = "\"left" + ("\\" * slashcount) + "\"" + (if (embedded) "right\"" else "")
          val expected = _following_lines("title = " + literal)
          val source = expected.mkString("\n")

          When("the script configuration groups the literal and JSON decodes the retained value")
          val result = LogicalLines.parse(LogicalLines.Config.script, source)
          val decoded = _decode_literal(result.lineVector.head.stripPrefix("title = "))

          Then("slash parity preserves lexical lines and the correct decoded quote and slash content")
          result.lineVector shouldBe expected
          decoded shouldBe "left" + ("\\" * (slashcount / 2)) + (if (embedded) "\"right" else "")
        }
      }

      "retain lexical and decoded Unicode display values for at least 100 cases" in {
        forAll(_display_string) { display =>
          Given("a padded nonblank Unicode display value forced to contain a quote and varying escape fragments")
          val literal = _encoded_literal(display)
          val expected = _following_lines("title = " + literal)
          val source = expected.mkString("\n")

          When("the lisp configuration groups the rendered JSON literal and JSON decodes its retained spelling")
          val result = LogicalLines.parse(LogicalLines.Config.lisp, source)
          val decoded = _decode_literal(result.lineVector.head.stripPrefix("title = "))

          Then("the actual line vector retains every encoded character and decoding recovers the exact display value")
          result.lineVector shouldBe expected
          decoded shouldBe display
        }
      }

      "retain ordinary document prologues and sibling ownership for at least 100 cases" in {
        forAll(_display_string, Gen.oneOf(true, false)) { (display, withlocation) =>
          Given("a rendered Unicode property literal and an ordinary document location configuration")
          val conf = if (withlocation) LogicalBlocks.Config.default.forLisp else LogicalBlocks.Config.noLocation.forLisp
          val literal = _encoded_literal(display)
          val source = _document(literal)

          When("the ordinary document parser groups and assigns the properties to their sections")
          val result = LogicalBlocks.parse(conf, source)
          val root = result.sections.head
          val decoded = _decode_literal(root.prologue.lines.lineVector.head.stripPrefix("title = "))

          Then("actual ordered sections and prologue literals preserve both lexical and decoded content")
          result.sections.map(_.nameForModel) shouldBe Vector("LOGICAL-UI")
          root.prologue.lines.lineVector shouldBe Vector("title = " + literal, "producerId = \"fixture\"")
          root.sections.map(_.nameForModel) shouldBe Vector("LIST", "DETAIL")
          root.sections.map(_.prologue.lines.lineVector) shouldBe Vector(
            Vector("title = " + literal, "screenId = \"list\""),
            Vector("title = \"詳細\"", "screenId = \"detail\"")
          )
          root.location shouldBe (if (withlocation) Some(ParseLocation(1, 1)) else None)
          decoded shouldBe display
        }
      }
    }
  }
}
