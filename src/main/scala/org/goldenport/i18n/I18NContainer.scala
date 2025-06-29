package org.goldenport.i18n

import scalaz._, Scalaz._
import java.util.Locale

/*
 * @since   Jun. 24, 2025
 * @version Jun. 26, 2025
 * @author  ASAMI, Tomoharu
 */
case class I18NContainer[T](
  c: T, // special locale for programming language
  en: T, // default
  ja: T,
  map: Map[Locale, T]
) {
  def get(locale: Locale): Option[T] = {
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

  def apply(locale: Locale): T = get(locale) getOrElse en

  def default = en
}

object I18NContainer {
  def make[T](p: T): I18NContainer[T] = I18NContainer(p, p, p, Map.empty)

  def create[T](p: Map[Locale, Seq[T]]): I18NContainer[List[T]] =
    _create(p.mapValues(_.toList))

  private def _create[T](p: Map[Locale, List[T]]): I18NContainer[List[T]] = {
    val copt = p.get(LocaleUtils.C)
    val enopt = p.get(LocaleUtils.en)
    val jaopt = p.get(LocaleUtils.ja)
    val (c, e, j) = (copt, enopt, jaopt) match {
      case (Some(c), Some(e), Some(j)) => (c, e, j)
      case (Some(c), Some(e), None) => (c, e, e)
      case (Some(c), None, Some(j)) => (c, j, j)
      case (Some(c), None, None) => (c, c, c)
      case (None, Some(e), Some(j)) => (e, e, j)
      case (None, Some(e), None) => (e, e, e)
      case (None, None, Some(j)) => (j, j, j)
      case (None, None, None) => (Nil, Nil, Nil)
    }
    val a = p -- Set(LocaleUtils.C, LocaleUtils.en, LocaleUtils.ja)
    I18NContainer(c, e, j, a)
  }
}
