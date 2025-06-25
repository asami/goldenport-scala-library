package org.goldenport.i18n

import scalaz._, Scalaz._
import java.util.Locale

/*
 * @since   Jun. 24, 2025
 * @version Jun. 24, 2025
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
}
