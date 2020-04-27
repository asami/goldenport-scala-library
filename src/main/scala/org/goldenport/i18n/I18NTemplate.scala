package org.goldenport.i18n

import java.util.Locale 

/*
 * @since   Feb. 28, 2017
 *  version Jul.  1, 2017
 *  version Aug. 28, 2017
 *  version Sep. 23, 2019
 * @version Apr. 17, 2020
 * @author  ASAMI, Tomoharu
 */
case class I18NTemplate(
  en: String,
  ja: String,
  map: Map[Locale, String]
) {
  def toI18NMessage(x: Any, xs: Any*): I18NMessage = toI18NMessage(x :: xs.toList)

  def toI18NMessage(parameters: Seq[Any]): I18NMessage =
    I18NMessage(en, en, ja, map, parameters.toVector)
}

object I18NTemplate {
  def apply(en: String, ja: String): I18NTemplate =
    I18NTemplate(en, ja, Map.empty)
}
