package org.goldenport.config

import com.typesafe.config.{ConfigFactory => HoconFactory}
import com.typesafe.config.{Config => Hocon}
import io.circe._
import io.circe.parser._
// import io.circe.syntax._
// import io.circe.generic.auto._
import org.goldenport.context.Consequence
import org.goldenport.i18n.I18NContext
import org.goldenport.io.InputSource
import org.goldenport.realm.Realm

/*
 * @since   Apr. 21, 2025
 *  version May. 23, 2025
 *  version Jun. 14, 2025
 *  version Jul.  5, 2025
 * @version Jun. 18, 2026
 * @author  ASAMI, Tomoharu
 */
object ConfigLoader {
  sealed trait Format
  object Format {
    case object Hocon extends Format
    case object Json extends Format
    case object Yaml extends Format
  }

  def loadConfig[T: Decoder](in: InputSource): Consequence[T] =
    StructuredDocumentLoader.loadDocument[T](in)

  def loadConfig[T: Decoder](in: InputSource, format: Format): Consequence[T] =
    StructuredDocumentLoader.loadDocument[T](in, _to_structured_format(format))

  private def _to_structured_format(format: Format): StructuredDocumentLoader.Format =
    format match {
      case Format.Hocon => StructuredDocumentLoader.Format.Hocon
      case Format.Json => StructuredDocumentLoader.Format.Json
      case Format.Yaml => StructuredDocumentLoader.Format.Yaml
    }

  def loadConfigFromYaml[T: Decoder](in: InputSource): Consequence[T] =
    loadConfig(in, Format.Yaml)

  def loadConfigHocon(in: InputSource): Consequence[Hocon] = {
    import org.goldenport.util.CirceUtils.Codec.hoconDecoder

    def _load_hocon_(): Consequence[Hocon] = Consequence run {
      val s = in.asText
      Consequence(HoconFactory.parseString(s))
    }

    _load_hocon_.toOption match {
      case Some(s) => Consequence.success(s)
      case None => loadConfig(in)
    }
  }

// import pureconfig._
// import pureconfig.generic.auto._

//   def loadConfig2[T: ConfigReader](name: String): Consequence[Option[T]] = {
//     val json: Hocon = ???
//     val source = ConfigSource.fromConfig(json)
//     source.load[T] match {
//       case Right(r) => ???
//       case Left(r) => ???
//     }
//   }

  def loadConfigJson(
    realm: Realm,
    pathname: String
  )(implicit ctx: I18NContext): Consequence[Json] = {
    val c = _parse(
      realm.getString(pathname + ".conf").
        map(io.circe.config.parser.parse)
    )
    val p = _parse(
      realm.getString(pathname + ".properties").
        map(io.circe.config.parser.parse)
    )
    val ps = _parse(
      realm.getString(pathname + ".props").
        map(io.circe.config.parser.parse)
    )
    val ya = _parse(
      realm.getString(pathname + ".yaml").
        map(io.circe.yaml.parser.parse)
    )
    val y: Consequence[Json] = _parse(
      realm.getString(pathname + ".yml").
        map(io.circe.yaml.parser.parse)
    )
    val j = _parse(
      realm.getString(pathname + ".json").
        map(io.circe.parser.parse)
    )
    for {
      js <- c
      jp <- p
      jps <- ps
      jya <- ya
      jy <- y
      jj <- j
      r <- _merge(jya, jy, js, jp, jps, jj)
//      r <- Consequence(jya.deepMerge(jy).deepMerge(js).deepMerge(jp).deepMerge(jps).deepMerge(jj))
    } yield r
  }

  private def _merge(jsons: Json*): Consequence[Json] = Consequence {
    jsons.toList.filterNot(_ == Json.Null) match {
      case Nil => Json.Null
      case xs => xs.reduce(_ deepMerge _)
    }
  }

  private def _parse(body: => Option[Either[ParsingFailure, Json]]): Consequence[Json] =
    Consequence run {
      body match {
        case Some(Right(r)) => Consequence.success(r)
        case Some(Left(l)) => Consequence.syntaxErrorFault(l.toString)
        case None => Consequence.success(Json.Null)
      }
    }
}
