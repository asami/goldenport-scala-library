package org.goldenport.i18n

import scalaz._, Scalaz._
import java.util.Locale
import java.text.MessageFormat
import play.api.libs.json._
import org.goldenport.Strings
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
 *  version Feb. 18, 2020
 *  version Mar. 30, 2020
 *  version Apr. 17, 2020
 *  version May.  4, 2020
 *  version Feb. 15, 2021
 *  version Apr. 29, 2021
 *  version May. 30, 2021
 *  version Jun. 19, 2021
 *  version Feb.  1, 2022
 * @version Dec.  8, 2022
 * @author  ASAMI, Tomoharu
 */
case class I18NString(
  c: String, // special locale for programming language
  en: String,
  ja: String,
  map: Map[Locale, String]
) {
  lazy val keyForModel: String = en.trim.toLowerCase
  lazy val nameForModel: String = en.trim

  def as(locale: Locale): String = get(locale) getOrElse en

  def get(locale: Locale): Option[String] = {
    val l = Option(locale.getLanguage)
    val country = Option(locale.getCountry)
    val variant = Option(locale.getVariant)
    l flatMap { lang =>
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
  }

  def apply(locale: Locale): String = get(locale) getOrElse en

  lazy val localeMap: Map[Locale, String] =
    map + (Locale.ENGLISH -> en) + (Locale.JAPANESE -> ja)
  lazy val localeList: List[(Locale, String)] = localeMap.toList
  lazy val localeVector: Vector[(Locale, String)] = localeMap.toVector

  lazy val values: Vector[String] = localeVector.map(_._2)

  private lazy val _keys = localeVector.map(_._2.toLowerCase)

  def containsKey(p: String): Boolean = _keys.contains(p.toLowerCase)
  def containsKey(ps: Seq[String]): Boolean = ps.exists(containsKey)

  def length(locale: Locale): Int = {
    val s = as(locale)
    s.codePointCount(0, s.length)
  }
  def size(locale: Locale): Int = as(locale).length

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
      s"${c}${delimiter}${rhs.c}",
      s"${en}${delimiter}${rhs.en}",
      s"${ja}${delimiter}${rhs.ja}",
      locales
    )
  }

  def appendAll(s: String): I18NString = I18NString(
    c + s,
    en + s,
    ja + s,
    map.mapValues(_ + s)
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
      s"${c}: ${p.c}",
      s"${en}: ${p.en}",
      s"${ja}: ${p.ja}",
      map./:(Z(p.map))(_+_).r
    )
  }

  // lazy val toRecordI18NString: RI18NString = RI18NString(
  //   _en, _ja, map.toList, parameters
  // )

  def update(f: String => String): I18NString = I18NString(
    f(c), f(en), f(ja), map.map(x => (x._1, f(x._2)))
  )

  lazy val toJson: JsObject = {
    val enmap: Map[String, JsString] = Map(Locale.ENGLISH.getLanguage -> JsString(en))
    val jamap: Map[String, JsString] = Map(Locale.JAPANESE.getLanguage -> JsString(ja))
    val lmap: Map[String, JsString] = map map {
      case (k, v) => k.toString -> JsString(v)
    }
    val a = lmap ++ enmap ++ jamap
    JsObject(a.toVector)
  }

  def toI18NMessage: I18NMessage = I18NMessage.create(this)
  def toI18NElement: I18NElement = I18NElement(this)

  lazy val toJsonString: String = toJson.toString

  override def toString() = s"I18NString(${Strings.cutstring(en, 128)})"
}

object I18NString {
  implicit def I18NStringMonoid = new Monoid[I18NString] {
    def zero = empty
    def append(lhs: I18NString, rhs: => I18NString) = lhs concat rhs
  }

  val empty = I18NString("")

  def apply(en: String, ja: String): I18NString = I18NString(en, en, ja, Map.empty)
  def apply(en: String): I18NString = I18NString(en, en, en, Map.empty)
  def apply(ps: Seq[(Locale, String)]): I18NString = {
    case class Z(map: Map[Locale, String] = Map.empty) {
      def r = I18NString(_c, _en, _ja, map)
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
    I18NString(s, s, s, a.toMap)
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

  def mkI18NString(ps: NonEmptyList[I18NString], infix: String): I18NString =
    mkI18NString(ps.list, infix)

  def mkI18NString(ps: Seq[I18NString], infix: String): I18NString = {
    @annotation.tailrec
    def _go_(z: I18NString, ps: List[I18NString]): I18NString = ps match {
      case Nil => z
      case x :: xs => _go_(z.appendAll(infix) + x, xs)
    }
    _go_(empty, ps.toList)
  }
}
