package org.goldenport.context

import java.math.MathContext
import java.nio.charset.Charset
import java.util.{Locale, TimeZone, Currency}
import org.goldenport.i18n.I18NContext
import org.goldenport.i18n.EmptyResourceBundle
import org.goldenport.i18n.CalendarFormatter
import org.goldenport.i18n.StringFormatter
import org.goldenport.observability.ObservabilityContext
import org.goldenport.notification.NotificationContext
import org.goldenport.io.FileTextResolver
import org.goldenport.io.FileResolver
import org.goldenport.util.TextResolver

/*
 * @since   Oct. 13, 2024
 *  version Apr. 28, 2025
 *  version Jun.  7, 2025
 * @version Sep. 12, 2025
 * @author  ASAMI, Tomoharu
 */
case class ContextFoundation(
  mathContext: MathContext,
  i18NContext: I18NContext,
  dateTimeContext: DateTimeContext,
  formatContext: FormatContext,
  observabilityContext: ObservabilityContext,
  notificationContext: NotificationContext,
  randomContext: RandomContext,
  fileTextResolver: FileTextResolver
) {
  def withI18NContext(p: I18NContext) = copy(i18NContext = p)
}

object ContextFoundation {
  trait Holder {
    def contextFoundation: ContextFoundation
    def mathContext: MathContext = contextFoundation.mathContext
    def i18NContext: I18NContext = contextFoundation.i18NContext
    def dateTimeContext: DateTimeContext = contextFoundation.dateTimeContext
    def formatContext: FormatContext = contextFoundation.formatContext
    def observabilityContext: ObservabilityContext = contextFoundation.observabilityContext
    def notificationContext: NotificationContext = contextFoundation.notificationContext
    def randomContext: RandomContext = contextFoundation.randomContext
    def fileTextResolver: FileTextResolver = contextFoundation.fileTextResolver
    def fileResolver: FileResolver = fileTextResolver.fileResolver
    def textResolver: TextResolver = fileTextResolver.textResolver
  }

  case class Parameters(
    mathContext: Option[MathContext] = None,
    i18nContext: Option[I18NContext] = None,
    dateTimeContext: Option[DateTimeContext] = None,
    formatContext: Option[FormatContext] = None
  )

  def default(): ContextFoundation = build(Parameters())

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
    val observabilitycontext = ObservabilityContext.default // TODO
    val notificationcontext = NotificationContext.default // TODO
    val randomcontext = RandomContext.default // TODO
    val filetextresolver = FileTextResolver.create() // TODO
    ContextFoundation(
      mathcontext,
      i18ncontext,
      datetimecontext,
      formatcontext,
      observabilitycontext,
      notificationcontext,
      randomcontext,
      filetextresolver
    )
  }
}
