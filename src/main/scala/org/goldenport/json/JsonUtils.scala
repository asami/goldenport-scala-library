package org.goldenport.json

import scala.xml._
import scala.concurrent.duration._
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
 *  version Sep.  2, 2017
 *  version Oct. 29, 2017
 *  version Aug. 30, 2018
 * @version Sep.  4, 2018
 * @author  ASAMI, Tomoharu
 */
object JsonUtils {
  def toInt(key: String, j: JsValue): Int =
    getInt(key, j) getOrElse raiseMissing(key)

  def getInt(key: String, j: JsValue): Option[Int] =
    (j \\ key).headOption.map(element =>
      element match {
        case _: JsUndefined => raiseMissing(key)
        case JsNull => raiseMissing(key)
        case JsBoolean(x) => raiseMismatch(key, element)
        case JsNumber(x) => x.toInt
        case JsString(x) => x.toInt
        case x: JsObject => raiseMismatch(key, element)
        case JsArray(xs) => raiseMismatch(key, element)
      }
    )

  def raiseMissing(key: String): Nothing =
    throw new IllegalArgumentException(s"Missing $key")

  def raiseMismatch(key: String, j: JsValue): Nothing =
    throw new IllegalStateException(s"Mismatch $key value = $j")

  def raiseUndefined(j: JsValue): Nothing =
    throw new IllegalStateException(s"JsUndefined: $j")

  def raiseNull(j: JsValue): Nothing =
    throw new IllegalStateException(s"JsNull: $j")

  def raiseJsboolean(j: JsValue): Nothing =
    throw new IllegalStateException(s"JsBoolean: $j")

  def raiseJsnumber(j: JsValue): Nothing =
    throw new IllegalStateException(s"JsNumber: $j")

  def raiseJsstring(j: JsValue): Nothing =
    throw new IllegalStateException(s"JsString: $j")

  def raiseJsobject(j: JsValue): Nothing =
    throw new IllegalStateException(s"JsObject: $j")

  def raiseJsarray(j: JsValue): Nothing =
    throw new IllegalStateException(s"JsArray: $j")

  def toString(key: String, j: JsValue): String =
    getString(key, j) getOrElse raiseMissing(key)

  def getString(key: String, j: JsValue): Option[String] =
    (j \\ key).headOption.map(element =>
      element match {
        case _: JsUndefined => raiseMissing(key)
        case JsNull => raiseMissing(key)
        case JsBoolean(x) => x.toString
        case JsNumber(x) => x.toString
        case JsString(x) => x
        case x: JsObject => raiseMismatch(key, element)
        case JsArray(xs) => raiseMismatch(key, element)
      }
    )

  def toString(j: JsValue): String = j match {
    case _: JsUndefined => raiseUndefined(j)
    case JsNull => raiseNull(j)
    case JsBoolean(x) => x.toString
    case JsNumber(x) => x.toString
    case JsString(x) => x
    case x: JsObject => raiseJsobject(j)
    case JsArray(xs) => raiseJsarray(j)
  }

  def toBoolean(j: JsValue): Boolean = j match {
    case _: JsUndefined => raiseUndefined(j)
    case JsNull => raiseNull(j)
    case JsBoolean(x) => x
    case JsNumber(x) => raiseJsnumber(j)
    case JsString(x) => raiseJsstring(j)
    case x: JsObject => raiseJsobject(j)
    case JsArray(xs) => raiseJsarray(j)
  }

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

  def toMapS(j: JsValue): Map[String, Any] = {
    j match {
      case o: JsObject => toMapS(o)
      case _ => throw new IllegalArgumentException(s"No JsObject = $j")
    }
  }

  def toMapS(j: JsObject): Map[String, Any] = {
    val a = j.value flatMap {
      case (k, v) => getValueS(v).map(x => k -> x)
    }
    Map.empty ++ a
  }

  def toMap[T](j: JsValue, f: JsValue => T): Map[String, T] = j match {
    case o: JsObject => o.value.map {
      case (k, v) => k -> f(v)
    }.toMap
    case _ => throw new IllegalArgumentException(s"No JsObject = $j")
  }

  def toMap[K, V](j: JsValue, fk: String => K, fv: JsValue => V): Map[K, V] = j match {
    case o: JsObject => o.value.map {
      case (k, v) => fk(k) -> fv(v)
    }.toMap
    case _ => throw new IllegalArgumentException(s"No JsObject = $j")
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

  def getValueS(j: JsValue): Option[Any] = j match {
    case _: JsUndefined => None
    case JsNull => None
    case JsBoolean(x) => Some(x)
    case JsNumber(x) => Some(x)
    case JsString(x) => Some(x)
    case x: JsObject => Some(toMapS(x))
    case JsArray(xs) => Some(xs.flatMap(getValueS))
  }

  def get(j: JsObject, key: String): Option[JsValue] = j.value.get(key)

  def getString(j: JsObject, key: String): Option[String] = getString(key, j)

  def getStringMap(j: JsObject, key: String): Option[Map[String, String]] =
    j.value.get(key).map(element =>
      element match {
        case _: JsUndefined => raiseMissing(key)
        case JsNull => raiseMissing(key)
        case JsBoolean(x) => raiseMismatch(key, element)
        case JsNumber(x) => raiseMismatch(key, element)
        case JsString(x) => raiseMismatch(key, element)
        case m: JsObject => m.value.map {
          case (k, v) => k -> toString(v)
        }.toMap
        case JsArray(xs) => raiseMismatch(key, element)
      }
    )

  /*
   * Json String
   */
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

  def parseDuration(s: String): FiniteDuration = AnyUtils.toFiniteDuration(s)

  def anyToJsValue(p: Any): JsValue = p match {
    case null => JsNull
    case None => JsNull
    case Some(x) => anyToJsValue(x)
    case x: String => JsString(x)
    case x: Boolean => JsBoolean(x)
    case x: Byte => JsNumber(x)
    case x: Short => JsNumber(x)
    case x: Int => JsNumber(x)
    case x: Long => JsNumber(x)
    case x: Float => JsNumber(x)
    case x: Double => JsNumber(x)
    case x: BigInt => JsNumber(BigDecimal(x))
    case x: BigDecimal => JsNumber(x)
    case x: java.sql.Timestamp => JsString(DateTimeUtils.toIsoDateTimeStringJst(x))
    case x: java.sql.Date => JsString(DateUtils.toIsoDateString(x))
    case x: java.util.Date => JsString(DateUtils.toIsoDateString(x))
    case x: DateTime => JsString(DateTimeUtils.toIsoDateTimeStringJst(x))
    case x: java.math.BigInteger => JsNumber(BigDecimal(x.toString))
//    case x: Record => toJsValue(x) // TODO IJsonStringable
//    case x: RecordSet => toJsValue(x)
    case xs: Seq[_] => JsArray(xs.map(anyToJsValue))
    case x: JsValue => x
    case x => JsString(x.toString)
  }

  def messageI18N(p: JsError): I18NString = {
    val a = p.errors.map {
      case (k, v) => s"""$k:${v.map(_.message).mkString("|")}"""
    }.mkString(";")
    I18NString(a)
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
    implicit object MapSymbolAnyFormat extends Format[Map[Symbol, Any]] {
      def reads(json: JsValue): JsResult[Map[Symbol, Any]] = json match {
        case m: JsObject => JsSuccess(toMap(m))
        case _ => JsError(s"Invalid StringMap($json)")
      }
      def writes(o: Map[Symbol, Any]): JsValue = JsObject(
        o.map {
          case (k, v) => k.name -> anyToJsValue(v)
        }.toList
      )
    }
    implicit object MapStringAnyFormat extends Format[Map[String, Any]] {
      def reads(json: JsValue): JsResult[Map[String, Any]] = json match {
        case m: JsObject => JsSuccess(toMapS(m))
        case _ => JsError(s"Invalid StringMap($json)")
      }
      def writes(o: Map[String, Any]): JsValue = JsObject(
        o.map {
          case (k, v) => k -> anyToJsValue(v)
        }.toList
      )
    }
    implicit val UrlFormat = new ValueFormat[URL](new URL(_), _.toString)
    implicit val UriFormat = new ValueFormat[URI](new URI(_), _.toString)
    implicit val FinitDurationFormat = new ValueFormat[FiniteDuration](
      parseDuration, _.toString
    )
    implicit object NodeSeqFormat extends Format[NodeSeq] {
      def reads(json: JsValue): JsResult[NodeSeq] = json match {
        case JsString(x) => JsSuccess(XML.load(x))
        case _ => JsError(s"Invalid Xml($json)")
      }
      def writes(o: NodeSeq): JsValue = JsString(o.toString)
    }
    implicit val I18NStringFormat = new ValueFormat[I18NString](
      I18NString.parse, _.toJsonString
    )
    implicit val I18NElementFormat = new ValueFormat[I18NElement](
      I18NElement.parse, _.toJsonString
    )
  }
}
