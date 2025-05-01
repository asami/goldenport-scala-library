package org.goldenport.i18n

import scala.util.control.NonFatal
import java.util.{Locale, Currency, TimeZone, ResourceBundle}
import java.nio.charset.Charset
import java.text.{NumberFormat, MessageFormat, DateFormat, DecimalFormat}
import org.joda.time._
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.values.{DateTimePeriod, Money, Percent}
import org.goldenport.util.AnyUtils

/*
 * @since   Aug.  4, 2019
 *  version Sep. 30, 2019
 *  version Oct. 18, 2019
 *  version Jan.  2, 2021
 *  version Feb. 20, 2021
 *  version Feb. 28, 2022
 *  version Dec. 10, 2022
 * @version Oct. 14, 2024
 * @author  ASAMI, Tomoharu
 */
case class I18NContext(
  charset: Charset,
  charsetInputFileOption: Option[Charset],
  charsetOutputFileOption: Option[Charset],
  charsetConsoleOption: Option[Charset],
  newline: String,
  locale: Locale,
  timezone: TimeZone,
  currency: Currency,
  calendarFormatters: CalendarFormatter.Factory,
  stringFormatter: StringFormatter,
  resourceBundle: ResourceBundle,
  databaseTimeZone: Option[TimeZone] = None
) {
  lazy val dateTimeZone = DateTimeZone.forTimeZone(timezone)
  lazy val effectiveDatabaseDateTimeZone = DateTimeZone.forTimeZone(databaseTimeZone getOrElse timezone)
  private lazy val _numberFormatter = NumberFormat.getNumberInstance(locale)
  private lazy val _intFormatter = NumberFormat.getIntegerInstance(locale)
  private lazy val _currencyFormatter = NumberFormat.getCurrencyInstance(locale)
  private lazy val _percentFormatter = NumberFormat.getPercentInstance(locale)

  def effectiveDatabaseTimeZone = databaseTimeZone getOrElse timezone

  def withLocalTimeZone(l: Locale, tz: TimeZone) = copy(
    locale = l,
    timezone = tz
  )

  def charsetInputFile = charsetInputFileOption getOrElse charset
  def charsetOutputFile = charsetOutputFileOption getOrElse charset
  def charsetConsole = charsetConsoleOption getOrElse charset

  def print(p: Any): String = p match {
    case m: Number => printNumber(m)
    case m: Money => printMoney(m)
    case m: Percent => printPercent(m)
    case m => format(m)
  }

  def format(p: Any): String = p match {
    case m: Number => formatNumber(m)
    case m: I18NString => m(locale)
    case m: java.sql.Timestamp => formatDateTime(m)
    case m: java.sql.Date => formatDate(m)
    case m: java.util.Date => formatDateTime(m)
    case m: DateTime => formatDateTime(m)
    case m: LocalDateTime => formatDateTime(m)
    case m: LocalDate => formatDate(m)
    case m: LocalTime => formatTime(m)
    case m: MonthDay => formatMonthDay(m)
    case m: DateTimePeriod => AnyUtils.toString(m) // TODO
    case m: Duration => AnyUtils.toString(m) // TODO
    case m: Period => AnyUtils.toString(m) // TODO
    case m: Money => formatMoney(m)
    case m: Percent => formatPercent(m)
    case m => AnyUtils.toString(m)
  }

  def formatNumber(p: Number): String = _numberFormatter.format(p)

  def formatDateTime(p: Long): String = calendarFormatters(locale, timezone).format(p)(this)

  def formatDateTime(p: java.sql.Timestamp): String = formatDateTime(p.getTime)

  def formatDate(p: java.util.Date): String = RAISE.notImplementedYetDefect

  def formatDateTime(p: java.util.Date): String = formatDateTime(p.getTime)

  def formatDateTime(p: DateTime): String = formatDateTime(p.getMillis)

  def formatDateTime(p: LocalDateTime): String = formatDateTime(p.toDateTime(dateTimeZone))

  def formatDate(p: LocalDate): String = RAISE.notImplementedYetDefect

  def formatTime(p: LocalTime): String = RAISE.notImplementedYetDefect

  def formatMonthDay(p: MonthDay): String = RAISE.notImplementedYetDefect

  def formatMoney(p: Money): String = RAISE.notImplementedYetDefect

  def formatPercent(p: Percent): String = RAISE.notImplementedYetDefect

  def printNumber(p: Number): String = AnyUtils.toString(p)

  def printMoney(p: Money): String = RAISE.notImplementedYetDefect

  def printPercent(p: Percent): String = RAISE.notImplementedYetDefect

  def format(fmt: String, p: Any): String = p match {
    case m: Number => formatNumber(fmt, m)
    case m: I18NString => formatString(fmt, m(locale))
    case m: java.sql.Timestamp => formatDateTime(fmt, m)
    case m: java.util.Date => formatDate(fmt, m)
    case m: DateTime => formatDateTime(fmt, m)
    case m: LocalDateTime => formatDateTime(fmt, m)
    case m: LocalDate => formatDate(fmt, m)
    case m: LocalTime => formatTime(fmt, m)
    case m: MonthDay => formatMonthDay(fmt, m)
    case m: DateTimePeriod => formatString(fmt, AnyUtils.toString(m)) // TODO
    case m: Duration => formatString(fmt, AnyUtils.toString(m)) // TODO
    case m: Period => formatString(fmt, AnyUtils.toString(m)) // TODO
    case m: Money => formatMoney(fmt, m)
    case m: Percent => formatPercent(fmt, m)
    case m => formatString(fmt, AnyUtils.toString(m))
  }

  def formatString(fmt: String, p: String): String = String.format(locale, fmt, p)

  def formatMessage(fmt: String, ps: Seq[Any]): String = MessageFormat.format(fmt, ps)

  def formatMessageKey(key: String, ps: Seq[Any]): String =
    Option(resourceBundle.getString(key)).
      map(formatMessage(_, ps)).
      getOrElse(formatMessage(key, ps))

  def formatMessageByKey(key: String, ps: Seq[Any]): String =
    Option(resourceBundle.getString(key)).
      map(formatMessage(_, ps)).
      getOrElse(RAISE.noSuchElementFault(key))

  def formatNumber(fmt: String, p: Number): String = {
    if (fmt.contains('o') || fmt.contains('x'))
      _format_number(fmt, p)
    else if (fmt.contains('#'))
      _format_decimal(fmt, p)
    else
      _format_number(fmt, p)
  }

  private def _format_number(fmt: String, p: Number): String = {
    val n = p match {
      case m: spire.math.Number => _format_normalize_number(fmt, m.toBigDecimal)
      case m: java.math.BigDecimal => scala.math.BigDecimal(m)
      case m => m
    }
    val fmt9 = _format_new_line(fmt)
    String.format(locale, s"%$fmt9", n)
  }

  private def _format_normalize_number(fmt: String, p: BigDecimal): Number =
    if (fmt.contains('d'))
      p.toBigInt.bigInteger
    else if (fmt.contains('f'))
      p.bigDecimal
    else
      p

  private def _format_new_line(fmt: String): String = fmt.replace("%n", newline)

  private def _format_decimal(fmt: String, p: Number): String = {
    val formatter = new DecimalFormat(fmt)
    formatter.format(p)
  }

  def formatDateTime(fmt: String, p: Long): String = formatDateTime(fmt, new DateTime(p, dateTimeZone))

  def formatDateTime(fmt: String, p: java.sql.Timestamp): String = formatDateTime(fmt, p.getTime)

  def formatDate(fmt: String, p: java.util.Date): String = RAISE.notImplementedYetDefect

  def formatDateTime(fmt: String, p: DateTime): String = calendarFormatters("???").format(fmt, p)(this)

  def formatDateTime(fmt: String, p: LocalDateTime): String = formatDateTime(fmt, p.toDateTime(dateTimeZone))

  def formatDate(fmt: String, p: LocalDate): String = RAISE.notImplementedYetDefect

  def formatTime(fmt: String, p: LocalTime): String = RAISE.notImplementedYetDefect

  def formatMonthDay(fmt: String, p: MonthDay): String = RAISE.notImplementedYetDefect

  def formatMoney(fmt: String, p: Money): String = RAISE.notImplementedYetDefect

  def formatPercent(fmt: String, p: Percent): String = RAISE.notImplementedYetDefect
}

object I18NContext {
  def CHARSET_UTF8 = CharsetUtils.UTF8
  val LINESEPARATOR_LF = "\n"
  val LINESEPARATOR_CR = "\r"
  val LINESEPARATOR_CRLF = "\r\n"
  val LOCALE_JA_JP = LocaleUtils.ja_JP
  val LOCALE_EN_US = LocaleUtils.en_US
  val LOCALE_EN_GB = LocaleUtils.en_GB
  val LOCALE_DE_DE = LocaleUtils.de_DE
  val LOCALE_DE_CH = LocaleUtils.de_CH

  val default = {
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
  val c = default.copy(locale = LocaleUtils.C)
  val test = {
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

  def apply(
    charset: Charset,
    newline: String,
    locale: Locale,
    timezone: TimeZone
  ): I18NContext = I18NContext(
    charset,
    None,
    None,
    None,
    newline,
    locale,
    timezone,
    Currency.getInstance(locale),
    CalendarFormatter.Factory.default,
    StringFormatter.default,
    default.resourceBundle
  )

  private def _country_map = Map(
    Locale.JAPANESE -> Locale.JAPAN
  )

  private val _default_currency = Currency.getInstance(Locale.US)

  private def _currency(locale: Locale): Currency = try {
    if (Strings.blankp(locale.getCountry))
      _country_map.get(locale).map(Currency.getInstance).getOrElse(_default_currency)
    else
      Currency.getInstance(locale)
  } catch {
    case NonFatal(e) => _default_currency
  }

  def apply(locale: Locale, tz: TimeZone): I18NContext =
    default.withLocalTimeZone(locale, tz)
}
