package org.goldenport.config

import java.net.URI
import io.circe.Decoder
import org.goldenport.io.StringInputSource
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

/*
 * @since   Jun. 18, 2026
 * @version Jun. 18, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class StructuredDocumentLoaderSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  import StructuredDocumentLoaderSpec._

  "StructuredDocumentLoader" should {
    "decode JSON, YAML, HOCON, and XML into the same model" in {
      val expected = SampleConfig("demo", Vector("one", "two"), Some("root summary"), Some("root text"))

      StructuredDocumentLoader.loadDocument[SampleConfig](_source("metadata.json", _json_text)).take should be(expected)
      StructuredDocumentLoader.loadDocument[SampleConfig](_source("metadata.yaml", _yaml_text)).take should be(expected)
      StructuredDocumentLoader.loadDocument[SampleConfig](_source("metadata.conf", _hocon_text)).take should be(expected)
      StructuredDocumentLoader.loadDocument[SampleConfig](_source("metadata.xml", _xml_text)).take should be(expected)
    }

    "convert repeated XML children to arrays" in {
      val json = StructuredDocumentLoader.loadJson(_source("metadata.xml", _xml_text)).take
      val items = json.hcursor.downField("item").as[Vector[String]].toOption.get

      items should be(Vector("one", "two"))
    }

    "prefer XML child elements over attributes when names collide" in {
      val xml =
        """<config name="attribute">
          |  <name>child</name>
          |</config>
          |""".stripMargin
      val json = StructuredDocumentLoader.loadJson(_source("metadata.xml", xml)).take

      json.hcursor.downField("name").as[String].toOption should be(Some("child"))
    }

    "keep mixed XML text in #text" in {
      val xml =
        """<config>
          |  mixed text
          |  <name>demo</name>
          |</config>
          |""".stripMargin
      val json = StructuredDocumentLoader.loadJson(_source("metadata.xml", xml)).take

      json.hcursor.downField("#text").as[String].toOption should be(Some("mixed text"))
      json.hcursor.downField("name").as[String].toOption should be(Some("demo"))
    }

    "reject singleton objects for arrays in strict non-XML formats" in {
      val json =
        """{
          |  "name": "demo",
          |  "summary": "root summary",
          |  "item": "one"
          |}
          |""".stripMargin
      val yaml =
        """name: demo
          |summary: root summary
          |item: one
          |""".stripMargin
      val hocon =
        """name = demo
          |summary = "root summary"
          |item = one
          |""".stripMargin

      StructuredDocumentLoader.loadDocument[SampleConfig](_source("metadata.json", json)).toOption should be(None)
      StructuredDocumentLoader.loadDocument[SampleConfig](_source("metadata.yaml", yaml)).toOption should be(None)
      StructuredDocumentLoader.loadDocument[SampleConfig](_source("metadata.conf", hocon)).toOption should be(None)
    }
  }

  private def _source(name: String, text: String): StringInputSource =
    StringInputSource(text, new URI(s"memory:///$name"))
}

object StructuredDocumentLoaderSpec {
  case class SampleConfig(
    name: String,
    item: Vector[String],
    summary: Option[String],
    `#text`: Option[String]
  )
  object SampleConfig {
    implicit val decoder: Decoder[SampleConfig] = Decoder.forProduct4("name", "item", "summary", "#text")(SampleConfig.apply)
  }

  private val _json_text: String =
    """{
      |  "name": "demo",
      |  "summary": "root summary",
      |  "item": ["one", "two"],
      |  "#text": "root text"
      |} """.stripMargin

  private val _yaml_text: String =
    """name: demo
      |summary: root summary
      |item:
      |  - one
      |  - two
      |"#text": root text
      |""".stripMargin

  private val _hocon_text: String =
    """name = demo
      |summary = "root summary"
      |item = [one, two]
      |"#text" = "root text"
      |""".stripMargin

  private val _xml_text: String =
    """<config name="demo" summary="root summary">
      |  root text
      |  <item>one</item>
      |  <item>two</item>
      |</config>
      |""".stripMargin
}
