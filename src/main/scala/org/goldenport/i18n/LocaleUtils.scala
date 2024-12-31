package org.goldenport.i18n

import java.util.{Locale, ResourceBundle}
import org.goldenport.Strings.totoken

/*
 * @since   Feb. 10, 2017
 *  version Aug. 29, 2017
 *  version Sep. 23, 2019
 * @version Oct. 14, 2024
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

  val C = new Locale("C")
  val ja_JP = new Locale("ja", "JP")
  val en_US = new Locale("en", "US")
  val en_GB = new Locale("en", "GB")
  val de_DE = new Locale("de", "DE")
  val de_CH = new Locale("de", "CH")
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
}
