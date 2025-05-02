package org.goldenport.config

import scala.collection.JavaConverters._
import com.typesafe.config.{ConfigFactory => HoconFactory, ConfigRenderOptions}
import io.circe._
import io.circe.parser._
import io.circe.syntax._
import io.circe.generic.auto._
import org.yaml.snakeyaml.Yaml
import org.goldenport.context.Consequence
import org.goldenport.io.InputSource
import org.goldenport.util.StringUtils
import org.goldenport.util.CirceUtils

/*
 * @since   Apr. 21, 2025
 *  version Apr. 21, 2025
 * @version May.  2, 2025
 * @author  ASAMI, Tomoharu
 */
object ConfigLoader {
  sealed trait Format
  object Format {
    case object Hocon extends Format
    case object Json extends Format
    case object Yaml extends Format
  }

  private def _detect_format(path: String): Option[Format] =
    StringUtils.getSuffix(path) collect {
      case "conf" => Format.Hocon
      case "json" => Format.Json
      case "yaml" => Format.Yaml
      case "yml" => Format.Yaml
    }

  def loadConfig[T: Decoder](in: InputSource): Consequence[T] = ???

  def loadConfig[T: Decoder](in: InputSource, format: Format): Consequence[T] = {
    def _load_hocon_(): Consequence[T] = Consequence run {
      val s = in.asText
      val conf = HoconFactory.parseString(s)
      val json = conf.root().render(ConfigRenderOptions.concise().setJson(true))
      _parse_json_(json)
    }

    def _load_json_(): Consequence[T] = Consequence run {
      val s = in.asText
      _parse_json_(s)
    }

    def _load_yaml_(): Consequence[T] = Consequence run {
      val yaml = new Yaml()
      val raw = yaml.load(in.openInputStream).asInstanceOf[java.util.Map[String, Object]]
      val json = CirceUtils.convertToJson(raw)
      Consequence.from(json.as[T])
    }

    def _parse_json_(p: String): Consequence[T] = Consequence.from(decode[T](p))

    format match {
      case Format.Hocon => _load_hocon_
      case Format.Json => _load_json_
      case Format.Yaml => _load_yaml_
    }
  }
}
