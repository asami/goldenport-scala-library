package org.goldenport.i18n

import java.util.Locale 

/*
 * @since   Feb. 28, 2017
 *  version Jul.  1, 2017
 *  version Aug. 28, 2017
 *  version Sep. 23, 2019
 *  version Apr. 17, 2020
 *  version Mar. 27, 2021
 *  version Apr. 29, 2021
 * @version May. 11, 2025
 * @author  ASAMI, Tomoharu
 */
case class I18NTemplate(
  c: String, // special locale for programming language
  en: String,
  ja: String,
  map: Map[Locale, String]
) {
  def appendAll(s: String): I18NTemplate = I18NTemplate(
    c + s,
    en + s,
    ja + s,
    map.mapValues(_ + s)
  )

  def prependAll(s: String): I18NTemplate = I18NTemplate(
    s + c,
    s + en,
    s + ja,
    map.mapValues(x => s + x)
  )

  def toI18NMessage(x: Any, xs: Any*): I18NMessage = toI18NMessage(x :: xs.toList)

  def toI18NMessage(parameters: Seq[Any]): I18NMessage =
    I18NMessage(c, en, ja, map, parameters.toVector)
}

object I18NTemplate {
  def apply(p: I18NMessage): I18NTemplate = apply(p.toI18NString)

  def apply(p: I18NString): I18NTemplate = I18NTemplate(p.c, p.en, p.ja, p.map)

  def apply(c: String): I18NTemplate = I18NTemplate(c, c, c, Map.empty)

  def apply(en: String, ja: String): I18NTemplate = I18NTemplate(en, en, ja, Map.empty)
}
