package org.goldenport.context

import java.util.{Locale, TimeZone}
import org.joda.time._
import org.goldenport.util.{DateTimeFormatter, DateFormatter, TimeFormatter}

/*
 * @since   Dec. 22, 2020
 * @version May.  3, 2022
 * @author  ASAMI, Tomoharu
 */
case class FormatContext(
  datetime: DateTimeFormatter = DateTimeFormatter.default,
  date: DateFormatter = DateFormatter.default,
  time: TimeFormatter = TimeFormatter.default
) {
  def formatDateTime(p: DateTime): String = datetime.format(p)
  def formatDateTime(p: Long): String = datetime.format(p)
  def formatDate(p: DateTime): String = date.format(p)
  def formatDate(p: Long): String = date.format(p)
  def formatTime(p: DateTime): String = time.format(p)
  def formatTime(p: Long): String = time.format(p)
}

object FormatContext {
  val default = FormatContext()

  def create(
    locale: Locale,
    tz: TimeZone
  ): FormatContext = FormatContext(
    DateTimeFormatter.create(locale, tz),
    DateFormatter.create(locale, tz),
    TimeFormatter.create(locale, tz)
  )

  def create(
    locale: Locale,
    tz: DateTimeZone
  ): FormatContext = FormatContext(
    DateTimeFormatter.create(locale, tz),
    DateFormatter.create(locale, tz),
    TimeFormatter.create(locale, tz)
  )
}

// case class FormatterContext(
//   datetime: DateTimeFormatter,
//   date: DateTimeFormatter,
//   time: DateTimeFormatter
// ) {
//   def withLocale(locale: Locale) = FormatterContext(
//     datetime.withLocale(locale),
//     date.withLocale(locale),
//     time.withLocale(locale)
//   )
// }
// object FormatterContext {
//   val default = FormatterContext(
//     DateTimeFormat.mediumDateTime().withLocale(Locale.ENGLISH),
//     DateTimeFormat.mediumDate().withLocale(Locale.ENGLISH),
//     DateTimeFormat.mediumTime().withLocale(Locale.ENGLISH)
//   )

//   def create(locale: Locale, style: String): FormatterContext =
//     FormatterContext(
//       DateTimeFormat.forStyle(style).withLocale(locale),
//       DateTimeFormat.forStyle(style).withLocale(locale),
//       DateTimeFormat.forStyle(style).withLocale(locale)
//     )

//   def createStyle(style: String): FormatterContext =
//     FormatterContext(
//       DateTimeFormat.forStyle(style),
//       DateTimeFormat.forStyle(style),
//       DateTimeFormat.forStyle(style)
//     )
// }
