package org.goldenport.util

import collection.JavaConverters._
import scala.util.Try
import scala.util.control.NonFatal
import scala.util.matching.Regex
import scalaz.NonEmptyList
import java.net.URL
import java.net.URI
import java.util.Locale
import com.typesafe.config.{Config => Hocon}
import com.typesafe.config.{ConfigFactory => HoconFactory}
import com.typesafe.config.{ConfigException}
import org.joda.time.DateTime
import org.joda.time.LocalDate
import io.circe._
import io.circe.yaml.syntax._
import org.goldenport.collection.NonEmptyVector
import org.goldenport.context.Consequence
import org.goldenport.context.{DateTimeContext => CDateTimeContext}
import org.goldenport.i18n.I18NContext

/*
 * @since   Apr. 21, 2025
 *  version Apr. 27, 2025
 *  version May. 24, 2025
 * @version Jun. 28, 2025
 * @author  ASAMI, Tomoharu
 */
object CirceUtils {
  // Scala 2.10
  def convertToJson(value: Any): Json = value match {
    case m: Json => m
    case m: java.util.Map[_, _] =>
      val scalaMap = m.asInstanceOf[java.util.Map[String, Object]].asScala.toMap
      Json.obj(
        scalaMap.map { case (k, v) => k -> convertToJson(v) }.toSeq: _*
      )
    case l: java.util.List[_] =>
      Json.fromValues(l.asScala.toSeq.map(convertToJson))
    case s: String => Json.fromString(s)
    case n: java.lang.Number => Json.fromBigDecimal(BigDecimal(n.toString))
    case b: java.lang.Boolean => Json.fromBoolean(b)
    case null => Json.Null
    case other => Json.fromString(AnyUtils.toString(other))
  }

  def toYamlString(p: Json): String = p.asYaml.spaces2

  def encode(p: (String, Any), ps: (String, Any)*): List[(String, Json)] =
    encode(p +: ps)

  def encode(ps: Seq[(String, Any)]): List[(String, Json)] = {
    case class Z(z: Vector[(String, Json)] = Vector.empty) {
      def r = z.toList

      def +(rhs: (String, Any)) = {
        val (k, v) = rhs
        _handle(k, v)
      }

      private def _handle(key: String, value: Any): Z = {
        value match {
          case m: Json => _add(key, m)
          case Some(s) => _handle(key, s)
          case None => _skip
          case m: Seq[_] =>
            val a = m.flatMap(_to_json)
            if (a.isEmpty)
              _skip
            else
              _add(key, Json.arr(a: _*))
          case m: NonEmptyList[_] => _handle(key, m.list)
          case m: NonEmptyVector[_] => _handle(key, m.vector)
          case m => _add(key, convertToJson(m))
        }
      }

      private def _to_json(p: Any): Option[Json] = Some(convertToJson(p))

      private def _add(key: String, value: Json) = copy(z = z :+ (key -> value))

      private def _skip = this
    }
    ps.toList.foldLeft(Z())(_+_).r
  }

  def toJson(p: (String, Any), ps: (String, Any)*): Json = {
    val fields = encode(p +: ps)
    Json.obj(fields: _*)
  }

  def toYamlString(p: (String, Any), ps: (String, Any)*): String = {
    val json = toJson(p, ps: _*)
    toYamlString(json)
  }

  def prefixedEncoder[A](prefix: String)(implicit enc: Encoder.AsObject[A]): Encoder[A] =
    Encoder.instance { a =>
      val obj = enc.encodeObject(a)
      Json.obj(
        obj.toMap.map { case (k, v) => s"$prefix$k" -> v }.toSeq: _*
      )
    }

  object Codec {
    implicit val urlEncoder: Encoder[URL] = Encoder.encodeString.contramap[URL](_.toString)

    implicit val urlDecoder: Decoder[URL] = Decoder.decodeString.emap { str =>
      Try(new URI(str).toURL).toEither.left.map(_.getMessage)
    }

    implicit val uriEncoder: Encoder[URI] = Encoder.encodeString.contramap[URI](_.toString)

    implicit val uriDecoder: Decoder[URI] = Decoder.decodeString.emap { str =>
      Try(new URI(str)).toEither.left.map(_.getMessage)
    }

    implicit val localeEncoder: Encoder[Locale] = Encoder.encodeString.contramap[Locale](_.toLanguageTag)

    implicit val localeDecoder: Decoder[Locale] = Decoder.decodeString.emap { str =>
      Try(Locale.of(str)).toEither.left.map(_.getMessage)
    }

    implicit val regexDecoder: Decoder[Regex] = Decoder.decodeString.emap { str =>
      try {
        Right(str.r)
      } catch {
        case NonFatal(e) => Left(s"Invalid regex: ${e.getMessage}")
      }
    }

    implicit val regexEncoder: Encoder[Regex] = Encoder.encodeString.contramap(_.regex)

    implicit def datetimeDecoder(implicit dctx: CDateTimeContext): Decoder[DateTime] =
      Decoder.decodeString.emap { s =>
        Consequence(DateTimeUtils.parseDateTime(s, dctx.dateTimeZone)).toEitherString
      }

    implicit val datetimeEncoder: Encoder[DateTime] = Encoder.encodeString.contramap(AnyUtils.toString)

    implicit def localdateDecoder(implicit dctx: CDateTimeContext): Decoder[LocalDate] =
      Decoder.decodeString.emap { s =>
        LocalDateUtils.consequenceLocalDate(s).toEitherString
      }

    implicit val localdateEncoder: Encoder[LocalDate] = Encoder.encodeString.contramap(AnyUtils.toString)

    def localdateFormatEncoder(implicit ctx: I18NContext): Encoder[LocalDate] =
      Encoder.encodeString.contramap(ctx.formatDate)

    implicit val hoconDecoder: Decoder[Hocon] = Decoder.instance { c =>
      c.as[Json].flatMap { json =>
        val str = json.noSpaces
        try {
          val hocon = HoconFactory.parseString(s"root = $str").getConfig("root")
          Right(hocon)
        } catch {
          case e: ConfigException => Left(DecodingFailure(e.getMessage, c.history))
        }
      }
    }
  }
}
