package org.goldenport.util

import java.util._
import java.nio.charset.Charset
import org.joda.time._
import org.goldenport.i18n.I18NContext
import org.goldenport.i18n.CalendarFormatter

/*
 * @since   Sep. 13, 2019
 * @version Sep. 25, 2019
 * @author  ASAMI, Tomoharu
 */
case class DateTimeContext(
  i18n: I18NContext,
  datetime: DateTime
) {
  def timezone = i18n.timezone
}

object DateTimeContext {
  def apply(
    charset: Charset,
    newline: String,
    locale: Locale,
    timezone: TimeZone,
    now: DateTime
  ): DateTimeContext = DateTimeContext(
    I18NContext(charset, newline, locale, timezone),
    now
  )
}
