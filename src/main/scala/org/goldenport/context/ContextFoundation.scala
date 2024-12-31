package org.goldenport.context

import java.math.MathContext
import java.nio.charset.Charset
import java.util.{Locale, TimeZone, Currency}
import org.goldenport.i18n.I18NContext
import org.goldenport.i18n.EmptyResourceBundle
import org.goldenport.i18n.CalendarFormatter
import org.goldenport.i18n.StringFormatter

/*
 * @since   Oct. 13, 2024
 * @version Oct. 13, 2024
 * @author  ASAMI, Tomoharu
 */
case class ContextFoundation(
  mathContext: MathContext,
  i18nContext: I18NContext,
  dateTimeContext: DateTimeContext,
  formatContext: FormatContext
) {
  def withI18NContext(p: I18NContext) = copy(i18nContext = p)
}

object ContextFoundation {
  trait Holder {
    def contextFoundation: ContextFoundation
    def mathContext: MathContext = contextFoundation.mathContext
    def i18nContext: I18NContext = contextFoundation.i18nContext
    def dateTimeContext: DateTimeContext = contextFoundation.dateTimeContext
    def formatContext: FormatContext = contextFoundation.formatContext
  }

  case class Parameters(
    mathContext: Option[MathContext] = None,
    i18nContext: Option[I18NContext] = None,
    dateTimeContext: Option[DateTimeContext] = None,
    formatContext: Option[FormatContext] = None
  )

  val default: ContextFoundation = build(Parameters())

  def build(params: Parameters): ContextFoundation = {
    val mathcontext = params.mathContext getOrElse MathContext.UNLIMITED // Scala default: MathContext.DECIMAL128
    val i18ncontext = params.i18nContext getOrElse {
      val charset = Charset.defaultCharset()
      val newline = System.lineSeparator()
      val locale = Locale.getDefault()
      val timezone = TimeZone.getDefault()
      val currency = Currency.getInstance(locale)
      val calenderformatters = CalendarFormatter.Factory.default
      val stringformatter = StringFormatter.default
      val bundle = EmptyResourceBundle
      I18NContext(
        charset,
        None,
        None,
        None,
        newline,
        locale,
        timezone,
        currency,
        calenderformatters,
        stringformatter,
        bundle
      )
    }
    val datetimecontext = DateTimeContext.now(i18ncontext.timezone)
    val formatcontext = FormatContext.create(i18ncontext.locale, i18ncontext.timezone)
    ContextFoundation(
      mathcontext,
      i18ncontext,
      datetimecontext,
      formatcontext
    )
  }
}