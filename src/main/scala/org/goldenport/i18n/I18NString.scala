package org.goldenport.i18n

import scalaz._, Scalaz._
import java.util.{Locale, ResourceBundle}
import java.text.MessageFormat
import play.api.libs.json._
import org.goldenport.util.{AnyUtils, AnyRefUtils}

/*
 * @since   Feb.  7, 2017
 *  version Apr.  5, 2017
 *  version May. 25, 2017
 *  version Jul. 11, 2017
 *  version Aug. 29, 2017
 *  version Sep.  1, 2017
 *  version Apr. 30, 2019
 *  version Jun.  8, 2019
 *  version Aug. 16, 2019
 *  version Sep. 23, 2019
 * @version Feb. 18, 2020
 * @author  ASAMI, Tomoharu
 */
case class I18NString(
  private[i18n] val _c: String, // special locale for programming language
  private[i18n] val _en: String,
  private[i18n] val _ja: String,
  map: Map[Locale, String],
  parameters: IndexedSeq[Any]
) {
  def key: String = en

  // TODO escape '{' and '}' or migrate to I18NTemplate
  def as(locale: Locale): String = get(locale) getOrElse _format(en)

  lazy val c = _format(_c)
  lazy val en = _format(_en)
  lazy val ja = _format(_ja)

  def get(locale: Locale): Option[String] = {
    val l = Option(locale.getLanguage)
    val country = Option(locale.getCountry)
    val variant = Option(locale.getVariant)
    val template: Option[String] = l flatMap { lang =>
      map.get(locale) orElse {
        // See com.asamioffice.goldenport.util.ULocale.match
        map.find(_._1.getLanguage === lang).map(_._2)
      }.orElse {
        if (lang === Locale.JAPANESE.getLanguage)
          Some(ja)
        else if (lang === Locale.ENGLISH.getLanguage)
          Some(en)
        else
          None
      }
    }
    _format(template)
  }

  private def _format(s: Option[String]): Option[String] = s.map(_format)

  private def _format(s: String): String =
    MessageFormat.format(s, parameters.map(AnyRefUtils.toAnyRef):_*)

  def get(locale: Locale, bundle: ResourceBundle): Option[String] =
    get(bundle) orElse get(locale)

  def get(bundle: ResourceBundle): Option[String] =
    _format(Option(bundle.getString(key)))

  def apply(locale: Locale): String = get(locale) getOrElse en

  def apply(locale: Locale, bundle: ResourceBundle): String =
    get(locale, bundle) getOrElse _format(en)

  // def formatMessage(locale: Locale, bundle: ResourceBundle)(ps: AnyRef*): String = {
  //   val fmt = apply(locale, bundle)
  //   MessageFormat.format(fmt, ps:_*)
  // }

  lazy val localeMap: Map[Locale, String] =
    map.mapValues(_format) +
      (Locale.ENGLISH -> en) + (Locale.JAPANESE -> ja)
  lazy val localeList: List[(Locale, String)] = localeMap.toList
  lazy val localeVector: Vector[(Locale, String)] = localeMap.toVector

  private lazy val _keys = localeVector.map(_._2.toLowerCase)

  def containsKey(p: String): Boolean = _keys.contains(p.toLowerCase)
  def containsKey(ps: Seq[String]): Boolean = ps.exists(containsKey)

  def +(rhs: I18NString): I18NString = concat(rhs, "")
  def concat(rhs: I18NString): I18NString = concat(rhs, ";")
  def concat(rhs: I18NString, delimiter: String): I18NString = {
    case class Z(r: Map[Locale, String]) {
      def +(rhs: (Locale, String)) = {
        val (l, s) = rhs
        r.get(l).fold(Z(r + rhs)) { x =>
          Z(r + (l -> s"${x}${delimiter}${s}"))
        }
      }
    }
    val locales = rhs.map./:(Z(map))(_+_).r
    I18NString(
      s"${_c}${delimiter}${rhs._c}",
      s"${_en}${delimiter}${rhs._en}",
      s"${_ja}${delimiter}${rhs._ja}",
      locales,
      parameters ++ rhs.parameters
    )
  }

  def appendAll(s: String): I18NString = I18NString(
    _c + s,
    _en + s,
    _ja + s,
    map.mapValues(_ + s),
    parameters
  )

  def appendDetail(p: I18NString): I18NString = {
    case class Z(
      remainder: Map[Locale, String],
      map: Map[Locale, String] = Map.empty
    ) {
      def r = map ++ remainder
      def +(rhs: (Locale, String)) = {
        val (l, s) = rhs
        remainder.get(l).fold {
          copy(map = map + (l -> s))
        } { x =>
          Z(remainder - l, map + (l -> s"${x}: ${s}"))
        }
      }
    }
    I18NString(
      s"${_c}: ${p.c}",
      s"${_en}: ${p.en}",
      s"${_ja}: ${p.ja}",
      map./:(Z(p.map))(_+_).r,
      parameters
    )
  }

  // lazy val toRecordI18NString: RI18NString = RI18NString(
  //   _en, _ja, map.toList, parameters
  // )

  def update(f: String => String): I18NString = I18NString(
    f(c), f(en), f(ja), map.map(x => (x._1, f(x._2))), parameters
  )

  lazy val toJson: JsObject = {
    val enmap: Map[String, JsString] = Map(Locale.ENGLISH.getLanguage -> JsString(_en))
    val jamap: Map[String, JsString] = Map(Locale.JAPANESE.getLanguage -> JsString(_ja))
    val lmap: Map[String, JsString] = map map {
      case (k, v) => k.toString -> JsString(v)
    }
    val pmap: Map[String, JsValue] = if (parameters.isEmpty) Map.empty else {
      Map("parameters" -> JsArray(parameters.map(x => JsString(AnyUtils.toString(x)))))
    }
    val a = lmap ++ enmap ++ jamap ++ pmap
    JsObject(a.toVector)
  }

  lazy val toJsonString: String = toJson.toString

  override def toString() = toJsonString
}

object I18NString {
  val empty = I18NString("")

  def apply(en: String, ja: String): I18NString = I18NString(en, en, ja, Map.empty, Vector.empty)
  def apply(en: String, params: Seq[Any]): I18NString = I18NString(en, en, en, Map.empty, params.toVector)
  def apply(en: String, ja: String, params: Seq[Any]): I18NString = I18NString(en, en, ja, Map.empty, params.toVector)
  def apply(en: String): I18NString = I18NString(en, en, en, Map.empty, Vector.empty)
  def apply(ps: Seq[(Locale, String)]): I18NString = {
    case class Z(map: Map[Locale, String] = Map.empty) {
      def r = I18NString(_c, _en, _ja, map, Vector.empty)
      def +(rhs: (Locale, String)) = Z(map + rhs)
      private def _c: String = {
        val xs = ps.toVector
        LocaleUtils.getC(xs).orElse(LocaleUtils.getEnglish(xs)).orElse(LocaleUtils.getJapanese(xs)).orElse(xs.headOption.map(_._2)).getOrElse("-")
      }
      private def _en: String = {
        val xs = ps.toVector
        LocaleUtils.getEnglish(xs).orElse(LocaleUtils.getJapanese(xs)).orElse(xs.headOption.map(_._2)).getOrElse("-")
      }
      private def _ja: String = {
        val xs = ps.toVector
        LocaleUtils.getJapanese(xs).orElse(LocaleUtils.getEnglish(xs)).orElse(xs.headOption.map(_._2)).getOrElse("-")
      }
    }
    ps.foldLeft(Z())(_+_).r
  }

  // def apply(p: RI18NString): I18NString = I18NString(p.en, p.ja, Map.empty, p.parameters)

  def create(locale: Locale, s: String): I18NString = {
    val a = if (LocaleUtils.isC(locale) || LocaleUtils.isEnglish(locale) || LocaleUtils.isJapanese(locale))
      List.empty
    else
      List((locale, s))
    I18NString(s, s, s, a.toMap, Vector.empty)
  }

  def parse(p: String): I18NString = {
    def parsejson = {
      Json.parse(p) match {
        case JsObject(ms) => // TODO parameters
          val a = for ((l, s) <- ms) yield (new Locale(l), s.toString)
          apply(a)
        case m => throw new IllegalArgumentException(s"I18NString#parse: $m")
      }
    }
    if (p.startsWith("{"))
      parsejson
    else
      I18NString(p)
  }

  // def concatRecordString(ps: Seq[RI18NString]): I18NString = concat(ps.map(apply))

  def concat(ps: Seq[I18NString]): I18NString = ps.toList match {
    case Nil => empty
    case x :: Nil => x
    case x :: xs => xs./:(x)(_ concat _)
  }

  def concatOption(ps: Seq[I18NString]): Option[I18NString] = ps.toList match {
    case Nil => None
    case x :: Nil => Some(x)
    case x :: xs => Some(xs./:(x)(_ concat _))
  }
}
