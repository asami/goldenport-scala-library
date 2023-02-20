package org.goldenport.i18n

import java.util.Locale
import java.util.TimeZone
import org.goldenport.context.DateTimeContext
import org.goldenport.util.AnyUtils

/*
 * See org.goldenport.context.FormatContext
 * 
 * @since   Dec. 10, 2022
 * @version Feb. 20, 2023
 * @author  ASAMI, Tomoharu
 */
case class FormatterContext(
  i18n: I18NContext,
  datetime: Option[DateTimeContext] = None
) {
  def timezone = i18n.timezone // datetime.timezone
  def dateTimeZone = i18n.dateTimeZone // datetime.dateTimeZone
  def locale = i18n.locale
  def effectiveDatabaseTimeZone = i18n.effectiveDatabaseTimeZone
  def effectiveDatabaseDateTimeZone = i18n.effectiveDatabaseDateTimeZone

  def toString(p: Any): String = AnyUtils.toString(p)
}

object FormatterContext {
  def now() = FormatterContext(I18NContext.default, Some(DateTimeContext.now))

  def apply(locale: Locale, tz: TimeZone): FormatterContext =
    FormatterContext(I18NContext(locale, tz))

  def apply(i18n: I18NContext, datetime: DateTimeContext): FormatterContext =
    FormatterContext(i18n, Some(datetime))
}

