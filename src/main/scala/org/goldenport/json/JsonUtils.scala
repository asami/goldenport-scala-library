package org.goldenport.json

import java.net.{URL, URI}
import java.sql.Timestamp
import org.joda.time.DateTime
import play.api.libs.json._
import play.api.libs.functional.syntax._
import org.goldenport.i18n.{I18NString, I18NElement}
import org.goldenport.util._

/*
 * @since   May. 23, 2014
 *  version Jun.  1, 2014
 *  version Dec. 28, 2014
 *  version Jan. 30, 2015
 *  version Nov.  4, 2015
 *  version Jan. 27, 2016
 *  version Apr.  4, 2017
 *  version Aug. 29, 2017
 * @version Sep.  2, 2017
 * @author  ASAMI, Tomoharu
 */
object JsonUtils {
  def toInt(key: String, j: JsValue): Int =
    getInt(key, j) getOrElse _throw_missing(key)

  def getInt(key: String, j: JsValue): Option[Int] =
    (j \\ key).headOption.map(element =>
      element match {
        case _: JsUndefined => _throw_missing(key)
        case JsNull => _throw_missing(key)
        case JsBoolean(x) => _throw_mismatch(key, element)
        case JsNumber(x) => x.toInt
        case JsString(x) => x.toInt
        case x: JsObject => _throw_mismatch(key, element)
        case JsArray(xs) => _throw_mismatch(key, element)
      }
    )

  private def _throw_missing(key: String): Nothing =
    throw new IllegalArgumentException(s"Missing $key")

  private def _throw_mismatch(key: String, j: JsValue): Nothing =
    throw new IllegalStateException(s"Mismatch $key value = $j")

  def toString(key: String, j: JsValue): String =
    getString(key, j) getOrElse _throw_missing(key)

  def getString(key: String, j: JsValue): Option[String] =
    (j \\ key).headOption.map(element =>
      element match {
        case _: JsUndefined => _throw_missing(key)
        case JsNull => _throw_missing(key)
        case JsBoolean(x) => _throw_mismatch(key, element)
        case JsNumber(x) => x.toString
        case JsString(x) => x
        case x: JsObject => _throw_mismatch(key, element)
        case JsArray(xs) => _throw_mismatch(key, element)
      }
    )

  def toMap(j: JsValue): Map[Symbol, Any] = {
    j match {
      case o: JsObject => toMap(o)
      case _ => throw new IllegalArgumentException(s"No JsObject = $j")
    }
  }

  def toMap(j: JsObject): Map[Symbol, Any] = {
    val a = j.value flatMap {
      case (k, v) => getValue(v).map(x => Symbol(k) -> x)
    }
    Map.empty ++ a
  }

  def toSeq(j: JsValue): Seq[JsValue] = {
    j match {
      case _: JsUndefined => Nil
      case JsNull => Nil
      case JsArray(xs) => xs
      case _ => Vector(j)
    }
  }

  def toVector(j: JsValue): Vector[JsValue] = {
    j match {
      case _: JsUndefined => Vector.empty
      case JsNull => Vector.empty
      case JsArray(xs) => xs.toVector
      case _ => Vector(j)
    }
  }

  def getValue(j: JsValue): Option[Any] = j match {
    case _: JsUndefined => None
    case JsNull => None
    case JsBoolean(x) => Some(x)
    case JsNumber(x) => Some(x)
    case JsString(x) => Some(x)
    case x: JsObject => Some(toMap(x))
    case JsArray(xs) => Some(xs.flatMap(getValue))
  }

  def seq2json(seq: Seq[(String, Any)]): String = {
    val buf = new StringBuilder
    seq2jsonObject(buf, seq)
    buf.toString
  }

  def seq2jsonObject(buf: StringBuilder, seq: Seq[(String, Any)]) {
    def tojson(xs: Seq[(String, Any)]) {
      xs.headOption.map { x =>
        if (isAvailableContent(x._2)) {
          seq2jsonContent(buf, x)
          seq2jsonContents(buf, xs.tail)
        } else {
          tojson(xs.tail)
        }
      }
    }

    buf.append("{")
    tojson(seq)
    buf.append("}")
  }

  def isAvailableContent(v: Any): Boolean = {
    v match {
      case None => false
      case _ => true
    }
  }

  def seq2jsonContents(buf: StringBuilder, seq: Seq[(String, Any)]) {
    for (data <- seq) {
      if (isAvailableContent(data._2)) {
        buf.append(", ")
        seq2jsonContent(buf, data)
      }
    }
  }

  def seq2jsonContentsComma(buf: StringBuilder, seq: Seq[(String, Any)]) {
    for (data <- seq) {
      if (seq2jsonContent(buf, data))
        buf.append(", ")
    }
  }

  def seq2jsonContent(buf: StringBuilder, data: (String, Any)): Boolean = {
    val (k, v) = data
    v match {
      case Some(s) => {
        seq2jsonContentValue(buf, k, s)
        true
      }
      case None => false
      case _ => {
        seq2jsonContentValue(buf, k, v)
        true
      }
    }
  }

  def seq2jsonContentValue(buf: StringBuilder, key : String, value: Any) {
    buf.append("\"")
    buf.append(key)
    buf.append("\"")
    buf.append(": ")
    data2json(buf, value)
  }

  def data2json(v: Any): String = {
    val buf = new StringBuilder
    data2json(buf, v)
    buf.toString
  }

  def data2json(buf: StringBuilder, v: Any) {
    def appendstring(s: String) {
      buf.append("\"")
      buf.append(escape(s))
      buf.append("\"")
    }
    v match {
      case _: Boolean => buf.append(v)
      case _: Byte => buf.append(v)
      case _: Short => buf.append(v)
      case _: Int => buf.append(v)
      case _: Long => buf.append(v)
      case _: Float => buf.append(v)
      case _: Double => buf.append(v)
      case _: BigInt => buf.append(v)
      case _: BigDecimal => buf.append(v)
      case ts: Timestamp => appendstring(DateTimeUtils.toIsoDateTimeStringJst(ts))
      case dt: DateTime => appendstring(DateTimeUtils.toIsoDateTimeStringJst(dt))
//      case d: Date => buf.append(DateTimeUtils.toString(ts))
//      case rec: Record => RecordAux.buildJsonString(rec, buf)
      case m: IJsonStringable => m.toJsonString(buf)
      case Some(s) => data2json(buf, s)
      case None => buf.append("null")
      case null => buf.append("null")
      case xs: Seq[_] => {
        buf.append("[")
        xs.headOption.map(x => {
          data2json(buf, x)
          for (a <- xs.tail) {
            buf.append(", ")
            data2json(buf, a)
          }
        })
        buf.append("]")
      }
      case _ => {
        buf.append("\"")
        buf.append(escape(v.toString))
        buf.append("\"")
      }
    }
  }

  def escapeOld(s: String): String = {
    if ((s.indexOf('"') == -1) && (s.indexOf('\\') == -1)) s
    else {
      val buf = new StringBuilder
      for (x: Char <- s) {
        x match {
          case '"' => buf.append("""\u0022""") // TODO same as escape_extjs
          case '\\' => buf.append("""\u005C""")
          case _ => buf.append(x)
        }
      }
      buf.toString
    }
  }

  def escape(s: String): String = {
    if ((s.indexOf('"') == -1) && (s.indexOf('\\') == -1) &&
      (s.indexOf('\n') == -1) && (s.indexOf('\r') == -1) &&
      (s.indexOf('\t') == -1)
    ) {
      s
    } else {
      val buf = new StringBuilder
      for (x: Char <- s) {
        x match {
          case '"' => buf.append("""\"""")
          case '\\' => buf.append("""\\""")
          case '\n' => buf.append("""\n""")
          case '\r' => buf.append("""\r""")
          case '\t' => buf.append("""\t""")
          case _ => buf.append(x)
        }
      }
      buf.toString
    }
  }

  class ValueFormat[T](fromf: String => T, tof: T => String) extends Format[T] {
    def reads(json: JsValue): JsResult[T] = {
      json match {
        case JsString(s) => JsSuccess(fromf(s))
        case _ => JsError(s"ValueFormat($json)")
      }
    }
    def writes(o: T): JsValue = JsString(tof(o))
  }
  object ValueFormat {
    def apply[T](fromf: String => T, tof: T => Option[String]): ValueFormat[T] =
      new ValueFormat(fromf, unlift(tof))
  }

  object Implicits {
    implicit val UrlFormat = new ValueFormat[URL](new URL(_), _.toString)
    implicit val UriFormat = new ValueFormat[URI](new URI(_), _.toString)
    implicit val I18NStringFormat = new ValueFormat[I18NString](
      I18NString.parse, _.toJsonString
    )
    implicit val I18NElementFormat = new ValueFormat[I18NElement](
      I18NElement.parse, _.toJsonString
    )
  }
}
