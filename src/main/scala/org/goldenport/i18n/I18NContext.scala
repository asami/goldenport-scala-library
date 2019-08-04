package org.goldenport.i18n

import java.util._
import java.nio.charset.Charset
import org.joda.time._

/*
 * @since   Aug.  4, 2019
 * @version Aug.  4, 2019
 * @author  ASAMI, Tomoharu
 */
case class I18NContext(
  charset: Charset,
  newline: String,
  locale: Locale,
  timezone: TimeZone
) {
  // TODO format
  // TODO MessageResource
}

object I18NContext {
}
