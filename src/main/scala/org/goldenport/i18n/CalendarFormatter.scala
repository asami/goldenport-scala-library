package org.goldenport.i18n

import java.util.{Locale, TimeZone}
import java.text.{SimpleDateFormat, DateFormat}
import org.joda.time._
import org.goldenport.collection.NonEmptyVector

/*
 * @since   Sep. 23, 2019
 * @version Sep. 25, 2019
 * @author  ASAMI, Tomoharu
 */
trait CalendarFormatter {
  def format(p: Long)(implicit ctx: I18NContext): String
  def format(fmt: String, p: DateTime)(implicit ctx: I18NContext): String
}

case class JavaCalendarFormatter(
  name: String,
  template: String,
  locale: Option[NonEmptyVector[Locale]] = None,
  style: CalendarFormatter.Style = CalendarFormatter.MediumStyle,
  scope: CalendarFormatter.Scope = CalendarFormatter.YearMonthDayHourMinuteSecond
) extends CalendarFormatter {
  def format(p: Long)(implicit ctx: I18NContext): String = {
    val fmt = new SimpleDateFormat(template)
    fmt.setTimeZone(ctx.timezone)
    fmt.format(ctx.locale, p)
  }

  def format(template: String, p: DateTime)(implicit ctx: I18NContext): String = {
    val tz = p.getZone.toTimeZone
    val t = p.getMillis
    val fmt = new SimpleDateFormat(template)
    fmt.setTimeZone(tz)
    fmt.format(ctx.locale, p)
  }
}
object JavaCalendarFormatter {
  // def apply(name: String, template: String): JavaCalendarFormatter =
  //   JavaCalendarFormatter(name, template, None, MediumStyle, YearMonthDayHourMinuteSecond)
}

object CalendarFormatter {
  sealed trait Style {
    def value: Int
  }
  object Style {

  }
  case object ShortStyle extends Style {
    val value = DateFormat.SHORT
  }
  case object MediumStyle extends Style {
    val value = DateFormat.MEDIUM
  }
  case object LongStyle extends Style {
    val value = DateFormat.LONG
  }
  case object Full extends Style {
    val value = DateFormat.FULL
  }

  sealed trait Scope {
  }

  case object YearMonthDayHourMinuteSecond extends Scope {
  }

  case object YearMonthDayHourMinute extends Scope {
  }

  case object YearMonthDayHour extends Scope {
  }

  case object YearMonthDay extends Scope {
  }

  case object YearMonth extends Scope {
  }

  case object Year extends Scope {
  }

  val ISO = JavaCalendarFormatter("iso", "yyyy-MM-ddTHH:mm:ssZ")
  val HTTP = JavaCalendarFormatter("http", "EEE, dd MMM yyyy HH:mm:ss z")
  val WEB = JavaCalendarFormatter("web", "yyyy/MM/dd HH:mm:ss")
  val JA_FULL = JavaCalendarFormatter("ja-full", "yyyy年M月d日H時m分")

  class Factory() {
    def apply(name: String): CalendarFormatter = WEB // TODO
    def apply(locale: Locale, tz: TimeZone): CalendarFormatter = WEB // TODO
  }

  object Factory {
    val default = new Factory()
  }
}
