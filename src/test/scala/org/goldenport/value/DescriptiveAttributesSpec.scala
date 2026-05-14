package org.goldenport.value

import java.net.URI
import java.util.Locale

import io.circe.parser.parse
import org.goldenport.io.StringInputSource
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

/*
 * @since   May. 14, 2026
 * @version May. 14, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class DescriptiveAttributesSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "DescriptiveAttributes" should {
    "read flat localized fields from JSON" in {
      val json = parse(
        """{
          |  "summary": "Default summary",
          |  "summary_i18n": {
          |    "ja": "日本語要約"
          |  }
          |}""".stripMargin
      ).toOption.get

      val attrs = DescriptiveAttributes.fromJson(json)

      attrs.summary.get(Locale.JAPANESE) should be(Some("日本語要約"))
      attrs.summary.get(Locale.ENGLISH) should be(Some("Default summary"))
    }

    "read nested descriptive_attributes fields from YAML" in {
      val yaml =
        """descriptive_attributes:
          |  summary: Default summary
          |  summary_i18n:
          |    ja: 日本語要約
          |""".stripMargin
      val in = StringInputSource(yaml, new URI("memory:///metadata.yaml"))

      val attrs = DescriptiveAttributes.load(in).take

      attrs.summary.get(Locale.JAPANESE) should be(Some("日本語要約"))
      attrs.summary.get(Locale.ENGLISH) should be(Some("Default summary"))
    }

    "prefer flat fields over nested descriptive_attributes" in {
      val json = parse(
        """{
          |  "summary": "Flat summary",
          |  "summary_i18n": {
          |    "ja": "フラット要約"
          |  },
          |  "descriptive_attributes": {
          |    "summary": "Nested summary",
          |    "summary_i18n": {
          |      "ja": "ネスト要約",
          |      "fr": "Résumé"
          |    }
          |  }
          |}""".stripMargin
      ).toOption.get

      val attrs = DescriptiveAttributes.fromJson(json)

      attrs.summary.get(Locale.ENGLISH) should be(Some("Flat summary"))
      attrs.summary.get(Locale.JAPANESE) should be(Some("フラット要約"))
      attrs.summary.get(Locale.FRENCH) should be(Some("Résumé"))
    }

    "merge with left-hand fields preferred" in {
      val lhs = DescriptiveAttributes(
        summary = DescriptiveAttributes.Text(Some("Flat summary"), Map("ja" -> "フラット要約"))
      )
      val rhs = DescriptiveAttributes(
        summary = DescriptiveAttributes.Text(Some("Nested summary"), Map("ja" -> "ネスト要約", "fr" -> "Résumé"))
      )

      val attrs = lhs.orElse(rhs)

      attrs.summary.get(Locale.ENGLISH) should be(Some("Flat summary"))
      attrs.summary.get(Locale.JAPANESE) should be(Some("フラット要約"))
      attrs.summary.get(Locale.FRENCH) should be(Some("Résumé"))
    }

    "convert localized text to I18NString without losing Japanese override" in {
      val text = DescriptiveAttributes.Text(
        Some("Default summary"),
        Map("ja" -> "日本語要約")
      )

      val i18n = text.toI18NString.get

      i18n.as(Locale.JAPANESE) should be("日本語要約")
      i18n.as(Locale.ENGLISH) should be("Default summary")
    }

    "not use English i18n as implicit Japanese fallback" in {
      val text = DescriptiveAttributes.Text(
        None,
        Map("en" -> "English summary")
      )

      val i18n = text.toI18NString("Fallback summary").get

      i18n.as(Locale.JAPANESE) should be("Fallback summary")
      i18n.as(Locale.ENGLISH) should be("English summary")
    }

    "read HOCON through ConfigLoader" in {
      val hocon =
        """summary = "Default summary"
          |summary_i18n.ja = "日本語要約"
          |""".stripMargin
      val in = StringInputSource(hocon, new URI("memory:///metadata.conf"))

      val attrs = DescriptiveAttributes.load(in).take

      attrs.summary.get(Locale.JAPANESE) should be(Some("日本語要約"))
      attrs.summary.get(Locale.ENGLISH) should be(Some("Default summary"))
    }
  }
}
