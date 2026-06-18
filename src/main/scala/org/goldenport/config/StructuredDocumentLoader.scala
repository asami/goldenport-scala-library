package org.goldenport.config

import scala.collection.JavaConverters._
import scala.util.control.NonFatal
import scala.xml.{Elem, Node, Text, XML}
import com.typesafe.config.{ConfigFactory => HoconFactory, ConfigRenderOptions}
import io.circe._
import io.circe.CursorOp
import io.circe.parser._
import org.yaml.snakeyaml.Yaml
import org.goldenport.context.Consequence
import org.goldenport.io.InputSource
import org.goldenport.util.StringUtils
import org.goldenport.util.CirceUtils

/*
 * @since   Jun. 18, 2026
 * @version Jun. 18, 2026
 * @author  ASAMI, Tomoharu
 */
object StructuredDocumentLoader {
  sealed trait Format
  object Format {
    case object Hocon extends Format
    case object Json extends Format
    case object Yaml extends Format
    case object Xml extends Format

    def detect(path: String): Option[Format] = {
      val suffix = StringUtils.getSuffix(path) orElse Some(path)
      suffix collect {
        case "conf" => Hocon
        case "hocon" => Hocon
        case "json" => Json
        case "yaml" => Yaml
        case "yml" => Yaml
        case "xml" => Xml
      }
    }
  }

  def loadJson(in: InputSource): Consequence[Json] =
    in.getSuffix.flatMap(Format.detect).map(loadJson(in, _)).getOrElse(loadJson(in, Format.Hocon))

  def loadJson(in: InputSource, format: Format): Consequence[Json] =
    format match {
      case Format.Hocon => _load_hocon(in)
      case Format.Json => _load_json(in)
      case Format.Yaml => _load_yaml(in)
      case Format.Xml => _load_xml(in)
    }

  def loadDocument[T: Decoder](in: InputSource): Consequence[T] = {
    val format = in.getSuffix.flatMap(Format.detect).getOrElse(Format.Hocon)
    loadDocument[T](in, format)
  }

  def loadDocument[T: Decoder](in: InputSource, format: Format): Consequence[T] =
    loadJson(in, format).flatMap(_decode[T](_, format))

  private def _decode[T: Decoder](json: Json, format: Format): Consequence[T] =
    format match {
      case Format.Xml => _decode_with_array_recovery[T](json, 0)
      case _ => Consequence.from(json.as[T])
    }

  private def _decode_with_array_recovery[T: Decoder](json: Json, count: Int): Consequence[T] =
    json.as[T] match {
      case Right(r) => Consequence.success(r)
      case Left(l) if count < 8 =>
        _array_recovery_candidates(l).iterator.map(_wrap_array(json, _)).collectFirst { case Some(s) => s } match {
          case Some(s) => _decode_with_array_recovery[T](s, count + 1)
          case None => Consequence.from(Left(l))
        }
      case Left(l) => Consequence.from(Left(l))
    }

  private def _array_recovery_candidates(failure: DecodingFailure): Vector[Vector[String]] =
    if (failure.message.contains("expecting array")) {
      val fields = failure.history.collect {
        case CursorOp.DownField(name) => name
      }.toVector
      Vector(fields, fields.reverse).filter(_.nonEmpty).distinct
    } else {
      Vector.empty
    }

  private def _wrap_array(json: Json, path: Vector[String]): Option[Json] =
    path.toList match {
      case Nil =>
        json.asArray match {
          case Some(_) => Some(json)
          case None => Some(Json.arr(json))
        }
      case key :: rest =>
        json.asObject.flatMap { obj =>
          obj(key).flatMap { value =>
            _wrap_array(value, rest.toVector).map { updated =>
              Json.fromJsonObject(obj.add(key, updated))
            }
          }
        }
    }

  private def _load_hocon(in: InputSource): Consequence[Json] = Consequence run {
    val s = in.asText
    val conf = HoconFactory.parseString(s)
    val json = conf.root().render(ConfigRenderOptions.concise().setJson(true))
    Consequence.from(parse(json))
  }

  private def _load_json(in: InputSource): Consequence[Json] = Consequence run {
    Consequence.from(parse(in.asText))
  }

  private def _load_yaml(in: InputSource): Consequence[Json] = Consequence run {
    val yaml = new Yaml()
    val raw = yaml.load[java.lang.Object](in.openInputStream)
    Consequence.success(CirceUtils.convertToJson(raw))
  }

  private def _load_xml(in: InputSource): Consequence[Json] = Consequence run {
    try {
      Consequence.success(_xml_root_to_json(XML.loadString(in.asText)))
    } catch {
      case NonFatal(e) => Consequence.syntaxErrorFault(Option(e.getMessage).getOrElse(e.toString))
    }
  }

  private def _xml_root_to_json(elem: Elem): Json =
    _xml_object(elem)

  private def _xml_node_to_json(node: Node): Json = {
    val children = _element_children(node)
    val attrs = node.attributes.asAttrMap
    val text = _text(node)
    if (attrs.isEmpty && children.isEmpty)
      text.map(Json.fromString).getOrElse(Json.obj())
    else
      _xml_object(node)
  }

  private def _xml_object(node: Node): Json = {
    val attrs = node.attributes.asAttrMap.toVector.sortBy(_._1).map {
      case (k, v) => k -> Json.fromString(v)
    }
    val childfields = _element_children(node).groupBy(_.label).toVector.sortBy(_._1).map {
      case (k, xs) =>
        val values = xs.map(_xml_node_to_json)
        val value = if (values.size == 1) values.head else Json.arr(values: _*)
        k -> value
    }
    val textfield = _text(node).filter(_ => childfields.nonEmpty || attrs.nonEmpty).map("#text" -> Json.fromString(_)).toVector
    Json.obj((attrs ++ textfield ++ childfields): _*)
  }

  private def _element_children(node: Node): Vector[Node] =
    node.child.collect { case e: Elem => e }.toVector

  private def _text(node: Node): Option[String] = {
    val s = node.child.collect { case Text(t) => t }.mkString.trim
    if (s.isEmpty) None else Some(s)
  }
}
