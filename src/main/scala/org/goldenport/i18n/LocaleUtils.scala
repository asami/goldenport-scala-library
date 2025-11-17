package org.goldenport.i18n

import java.util.{Locale, ResourceBundle}
import org.goldenport.Strings.totoken

/*
 * @since   Feb. 10, 2017
 *  version Aug. 29, 2017
 *  version Sep. 23, 2019
 *  version Oct. 14, 2024
 *  version Apr.  9, 2025
 *  version Jun. 24, 2025
 *  version Jul. 26, 2025
 * @version Nov. 13, 2025
 * @author  ASAMI, Tomoharu
 */
object LocaleUtils {
  def getC[T](ps: Map[Locale, T]): Option[T] = getC(ps.toVector)
  def getEnglish[T](ps: Map[Locale, T]): Option[T] = getEnglish(ps.toVector)
  def getJapanese[T](ps: Map[Locale, T]): Option[T] = getJapanese(ps.toVector)

  def getC[T](ps: Seq[(Locale, T)]): Option[T] = findByLanguage("C", ps)
  def getEnglish[T](ps: Seq[(Locale, T)]): Option[T] = findByLanguage("en", ps)
  def getJapanese[T](ps: Seq[(Locale, T)]): Option[T] = findByLanguage("ja", ps)

  def findByLanguage[T](lang: String, ps: Seq[(Locale, T)]): Option[T] = {
    case class Z(v: Option[(Locale, T)] = None) {
      def r = v.map(_._2)
      def +(rhs: (Locale, T)) = {
        val (locale, _) = rhs
        if (locale.getLanguage != lang) this else {
          v.fold(Z(Some(rhs))) { x =>
            if (_is_fit(rhs._1, x._1))
              Z(Some(rhs))
            else
              this
          }
        }
      }
      private def _is_fit(l: Locale, r: Locale) = {
        val a = Vector(totoken(l.getCountry), totoken(l.getVariant)).flatten
        val b = Vector(totoken(r.getCountry), totoken(r.getVariant)).flatten
        a.length < b.length
      }
    }
    ps.foldLeft(Z())(_+_).r
  }

  val C = Locale.forLanguageTag("C")
  val ja = Locale.forLanguageTag("ja")
  val en = Locale.forLanguageTag("en")
  val ja_JP = Locale.forLanguageTag("ja-JP")
  val en_US = Locale.forLanguageTag("en-US")
  val en_GB = Locale.forLanguageTag("en-GB")
  val de_DE = Locale.forLanguageTag("de-DE")
  val de_CH = Locale.forLanguageTag("de-CH")
  lazy val LANGUAGE_ENGLISH = Locale.ENGLISH.getLanguage
  lazy val LANGUAGE_JPAPNESE = Locale.JAPANESE.getLanguage

  def getByLocale(master: Seq[(Locale, String)])(locale: Locale): Option[String] =
    (Option(locale.getLanguage), Option(locale.getCountry), Option(locale.getVariant)) match {
      case (None, None, None) => None
      case (Some(l), None, None) => getByLanguage(master)(l)
      case (Some(l), Some(c), None) => getByLanguageCountry(master)(l, c)
      case (Some(l), Some(c), Some(v)) => getByLanguageCountryVariant(master)(l, c, v)
      case _ => None
    }

  def getByLanguage(master: Seq[(Locale, String)])(l: String): Option[String] =
    master.find(x => x._1.getLanguage == l).map(_._2)

  def getByLanguageCountry(master: Seq[(Locale, String)])(l: String, c: String): Option[String] =
    master.find(x => x._1.getLanguage == l && x._1.getCountry == c).map(_._2) orElse getByLanguage(master)(l)

  def getByLanguageCountryVariant(master: Seq[(Locale, String)])(l: String, c: String, v: String): Option[String] =
    master.find(x => x._1.getLanguage == l && x._1.getCountry == c && x._1.getVariant == v).map(_._2) orElse getByLanguageCountry(master)(l, c)

  def isC(locale: Locale): Boolean = locale == C
  def isEnglish(locale: Locale): Boolean = locale.getLanguage == LANGUAGE_ENGLISH
  def isJapanese(locale: Locale): Boolean = locale.getLanguage == LANGUAGE_JPAPNESE

  def isInclude(broader: Locale, specific: Locale): Boolean =
    broader.getLanguage == specific.getLanguage && {
      val country = broader.getCountry
      if (country.isEmpty) {
        true
      } else {
        country == specific.getCountry && {
          val variant = broader.getVariant
          if (variant.isEmpty)
            true
          else
            variant == specific.getVariant
        }
      }
    }

  def parse(s: String): Locale =
    if (s.contains("-")) {
      Locale.forLanguageTag(s)
    } else {
      val parts = s.split("_")
      val tag = parts.length match {
        case 1 => parts(0)
        case 2 => s"${parts(0)}-${parts(1)}"
        case 3 => s"${parts(0)}-${parts(1)}-${parts(2)}"
        case _ => throw new IllegalArgumentException(s"Invalid locale format: $s")
      }
      Locale.forLanguageTag(tag)
    }

  def getAvailableLocale(p: String): Option[Locale] = p match {
    case "ja" => Some(ja)
    case "en" => Some(en)
    case _ => None // TODO
  }

  /**
   * Check whether the base locale accepts the target locale.
   *
   * base   = the "accept" side (similar to Accept-Language preference)
   * target = the locale being evaluated
   *
   * Rules:
   * 1. If base language is wildcard ("*") → accept.
   * 2. If languages differ → reject.
   * 3. If base specifies a script → target must have the same script.
   * 4. If base specifies a country → target must have the same country.
   * 5. If base does not specify a country → any target country is accepted.
   */
  def isAccept(base: Locale, target: Locale): Boolean = {
    val baseLang = base.getLanguage
    val baseCountry = base.getCountry
    val baseScript = base.getScript
    val targetLang = target.getLanguage
    val targetCountry = target.getCountry
    val targetScript = target.getScript

    // Utility lambdas
    def isWildcard(s: String): Boolean = s == null || s.isEmpty || s == "*"

    // 1. Wildcard language
    if (isWildcard(baseLang)) return true

    // 2. Language must match
    if (baseLang != targetLang) return false

    // 3. Script check (only if base explicitly specifies it)
    if (!isWildcard(baseScript)) {
      if (baseScript != targetScript)
        return false
    }

    // 4. Country check (only if base explicitly specifies it)
    if (!isWildcard(baseCountry)) {
      if (baseCountry != targetCountry)
        return false
      else
        return true
    }

    // 5. Base has no country restriction → accept any
    true
  }
}
