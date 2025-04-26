package org.goldenport.util

import collection.JavaConverters._
import io.circe.Json
import io.circe.yaml.syntax._

/*
 * @since   Apr. 21, 2025
 * @version Apr. 25, 2025
 * @author  ASAMI, Tomoharu
 */
object CircleUtils {
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
          case m => _add(key, convertToJson(m))
        }
      }

      private def _to_json(p: Any): Option[Json] = Some(convertToJson(p))

      private def _add(key: String, value: Json) = copy(z = z :+ (key -> value))

      private def _skip = this
    }
    ps.toList./:(Z())(_+_).r
  }

  def toJson(p: (String, Any), ps: (String, Any)*): Json = {
    val fields = encode(p +: ps)
    Json.obj(fields: _*)
  }

  def toYamlString(p: (String, Any), ps: (String, Any)*): String = {
    val json = toJson(p, ps: _*)
    toYamlString(json)
  }
}
