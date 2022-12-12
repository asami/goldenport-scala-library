package org.goldenport.util

import java.util.{Locale, TimeZone}
import java.text.DateFormat
import java.text.SimpleDateFormat
import org.joda.time._
import org.joda.time.format.{DateTimeFormatter => JodaDateTimeFormatter}
import org.joda.time.format.DateTimeFormat
import org.joda.time.format.ISODateTimeFormat
import org.goldenport.parser._
import org.goldenport.value._
import org.goldenport.i18n.LocaleUtils
import org.goldenport.i18n.FormatterContext

/*
 * @since   May.  1, 2022
 *  version May.  3, 2022
 * @version Dec. 10, 2022
 * @author  ASAMI, Tomoharu
 */
trait DateTimeFormatter {
  def format(p: LocalDateTime): String
  def format(p: DateTime): String
  def format(p: java.util.Date): String = format(p.getTime)
  def format(p: java.sql.Date): String
  def format(p: java.sql.Timestamp): String = format(p.getTime)
  def format(p: Long): String
}

trait JavaDateTimeFormatter extends DateTimeFormatter with DateTimeFormatter.JavaFormatter {
  def format(p: LocalDateTime): String = format(p.toDateTime(dateTimeZone))
  def format(p: java.sql.Date): String = format(new DateTime(p.getTime, effectiveDatabaseDateTimeZone))
}

object DateTimeFormatter {
  trait JavaFormatter {
    def context: FormatterContext

    def timezone = context.timezone
    def dateTimeZone = context.dateTimeZone
    def effectiveDatabaseTimeZone = context.effectiveDatabaseTimeZone
    def effectiveDatabaseDateTimeZone = context.effectiveDatabaseDateTimeZone

    private def _sdf(p: DateTimeZone): DateFormat = java_Sdf(DateTimeUtils.dateTimeZoneToTz(p))

    private def _sdf_tz = java_Sdf(timezone)

    // thread unsafe
    protected def java_Sdf(p: TimeZone): DateFormat

    def format(p: DateTime) = _sdf(p.getZone).format(p.getMillis)
    def format(p: Long) = _sdf_tz.format(p)
  }

  trait JodaFormatterBase {
    def timezone: TimeZone

    private def _sdf(p: DateTimeZone): JodaDateTimeFormatter = joda_Sdf(DateTimeUtils.dateTimeZoneToTz(p))

    private def _sdf_tz = joda_Sdf(timezone)

    // thread safe
    protected def joda_Sdf(p: TimeZone): JodaDateTimeFormatter

    def format(p: DateTime) = _sdf(p.getZone).print(p.getMillis)
    def format(p: Long) = _sdf_tz.print(p)
  }

  sealed trait Style extends NamedValueInstance {
  }
  sealed trait JavaStyle extends Style {
  }
  sealed trait JodaStyle extends Style {
  }
  object Style extends EnumerationClass[Style] {
    val elements = Vector(
      Full, Long, Medium, Short,
      Iso, IsoFull, IsoBasic, IsoBasicFull, IsoBasicOrdinal, IsoBasicOrdinalFull,
      Http, Web, Simple, Natural
    )

    // Java SimpleDateStyle
    case object Full extends JavaStyle {
      val name = "full"
    }
    case object Long extends JavaStyle {
      val name = "long"
    }
    case object Medium extends JavaStyle {
      val name = "medium"
    }
    case object Short extends JavaStyle {
      val name = "short"
    }
    case object Iso extends JodaStyle {
      val name = "iso"
    }
    case object IsoFull extends JodaStyle {
      val name = "iso-full"
    }
    case object IsoBasic extends JodaStyle {
      val name = "iso-basic"
    }
    case object IsoBasicFull extends JodaStyle {
      val name = "iso-basic-full"
    }
    case object IsoBasicOrdinal extends JodaStyle {
      val name = "iso-basic-ordinal"
    }
    case object IsoBasicOrdinalFull extends JodaStyle {
      val name = "iso-basic-ordinal-full"
    }
    case object Http extends Style {
      val name = "http"
    }
    case object Web extends Style {
      val name = "web"
    }
    case object Simple extends Style {
      val name = "simple"
    }
    case object Natural extends Style {
      val name = "natural"
    }
    case class Pattern(pattern: String) extends Style {
      val name = "pattern"
    }
    val 月日時分 = Pattern("yyyy年M月d日H時m分s秒")

    override def get(p: String): Option[Style] =
      super.get(p).orElse(Some(Pattern(p)))

    override def getIgnoreCase(p: String): Option[Style] =
      super.getIgnoreCase(p).orElse(Some(Pattern(p)))

    override def parse(p: String): ParseResult[Style] =
      super.parse(p) match {
        case m: ParseSuccess[_] => m
        case EmptyParseResult() => ParseSuccess(Pattern(p))
        case ParseFailure(_, _) => ParseSuccess(Pattern(p))
      }
  }

  case class NamedJavaFormatter(
    context: FormatterContext,
    dateStyle: Int,
    timeStyle: Int
  ) extends JavaDateTimeFormatter {
    def tz = context.timezone

    protected def java_Sdf(p: TimeZone) = {
      val sdf = DateFormat.getDateTimeInstance(dateStyle, timeStyle, context.locale)
      sdf.setTimeZone(p)
      sdf
    }
  }
  object NamedJavaFormatter {
    def apply(
      l: Locale,
      tz: TimeZone,
      dateStyle: Int,
      timeStyle: Int
    ): NamedJavaFormatter = NamedJavaFormatter(
      FormatterContext(l, tz),
      dateStyle,
      timeStyle
    )
  }

  case class PatternJavaFormatter(
    context: FormatterContext,
    pattern: String
  ) extends JavaDateTimeFormatter {
    def tz = context.timezone

    protected def java_Sdf(p: TimeZone) = {
      val sdf = new SimpleDateFormat(pattern)
      sdf.setTimeZone(p)
      sdf
    }
  }
  object PatternJavaFormatter {
    def apply(
      l: Locale,
      tz: TimeZone,
      pattern: String
    ): PatternJavaFormatter = PatternJavaFormatter(FormatterContext(l, tz), pattern)
  }

  case class DateFormatFormatter(format: DateFormat) extends DateTimeFormatter {
    def format(p: LocalDateTime): String = format.format(p)
    def format(p: DateTime) = format.format(p.getMillis)
    def format(p: java.sql.Date): String = format.format(p)
    def format(p: Long) = format.format(p)
  }

  case class JodaFormatter(
    context: FormatterContext,
    formatter: JodaDateTimeFormatter
  ) extends DateTimeFormatter with JodaFormatterBase {
    def timezone = context.timezone
    def dateTimeZone = context.dateTimeZone
    def effectiveDatabaseTimeZone = context.effectiveDatabaseTimeZone
    def effectiveDatabaseDateTimeZone = context.effectiveDatabaseDateTimeZone

    protected def joda_Sdf(p: TimeZone) = formatter.withZone(DateTimeUtils.tzToDateTimeZone(p))

    def format(p: LocalDateTime): String = format(p.toDateTime(dateTimeZone))
    def format(p: java.sql.Date): String = format(new DateTime(p.getTime, effectiveDatabaseDateTimeZone))
  }
  object JodaFormatter {
    def apply(
      l: Locale,
      tz: TimeZone,
      formatter: JodaDateTimeFormatter
    ): JodaFormatter = JodaFormatter(FormatterContext(l, tz), formatter)
  }

  val default = _natural(Locale.ENGLISH, DateTimeUtils.gmt)

  def create(
    locale: Locale,
    tz: DateTimeZone
  ): DateTimeFormatter = create(locale, DateTimeUtils.dateTimeZoneToTz(tz))

  def create(
    locale: Locale,
    tz: TimeZone
  ): DateTimeFormatter = create(locale, tz, Style.Natural)

  def create(
    locale: Locale,
    tz: TimeZone,
    name: String
  ): DateTimeFormatter = {
    val style = Style(name)
    create(locale, tz, style)
  }

  def create(
    style: Style
  )(implicit ctx: FormatterContext): DateTimeFormatter =
    create(ctx.locale, ctx.timezone)

  def create(
    locale: Locale,
    tz: TimeZone,
    style: Style
  ): DateTimeFormatter = {
    style match {
      case Style.Full => NamedJavaFormatter(locale, tz, DateFormat.FULL, DateFormat.FULL)
      case Style.Long => NamedJavaFormatter(locale, tz, DateFormat.LONG, DateFormat.LONG)
      case Style.Medium => NamedJavaFormatter(locale, tz, DateFormat.MEDIUM, DateFormat.MEDIUM)
      case Style.Short => NamedJavaFormatter(locale, tz, DateFormat.SHORT, DateFormat.SHORT)
      case Style.Iso => JodaFormatter(locale, tz, DateTimeUtils.isoFormatter)
      case Style.IsoFull => JodaFormatter(locale, tz, ISODateTimeFormat.dateTime)
      case Style.IsoBasic => JodaFormatter(locale, tz, ISODateTimeFormat.basicDateTimeNoMillis)
      case Style.IsoBasicFull => JodaFormatter(locale, tz, ISODateTimeFormat.basicDateTime)
      case Style.IsoBasicOrdinal => JodaFormatter(locale, tz, ISODateTimeFormat.basicOrdinalDateTimeNoMillis)
      case Style.IsoBasicOrdinalFull => JodaFormatter(locale, tz, ISODateTimeFormat.basicOrdinalDateTime)
      case Style.Http => DateFormatFormatter(DateTimeUtils.httpDateTimeFormat)
      case Style.Web => PatternJavaFormatter(locale, tz, "yyyy/MM/dd HH:mm:ss")
      case Style.Simple => JodaFormatter(locale, tz, DateTimeUtils.simpleFormatter)
      case Style.Natural => _natural(locale, tz)
      case Style.Pattern(pattern) =>
        if (pattern.length == 2)
          JodaFormatter(locale, tz, DateTimeFormat.forStyle(pattern))
        else
          PatternJavaFormatter(locale, tz, pattern)
    }
  }

  private def _natural(
    locale: Locale,
    tz: TimeZone
  ) =
    if (LocaleUtils.isJapanese(locale))
      PatternJavaFormatter(FormatterContext(locale, tz), "M月d日H時m分")
    else
      NamedJavaFormatter(FormatterContext(locale, tz), DateFormat.MEDIUM, DateFormat.MEDIUM)

  private def _natural_year(
    locale: Locale,
    tz: TimeZone
  ) =
    if (LocaleUtils.isJapanese(locale))
      PatternJavaFormatter(locale, tz, "yyyy年M月d日H時m分")
    else
      NamedJavaFormatter(locale, tz, DateFormat.MEDIUM, DateFormat.MEDIUM)

  def format(
    style: Style,
    p: Any
  )(implicit ctx: FormatterContext): String = {
    val f = create(ctx.locale, ctx.timezone, style)
    p match {
      case m: LocalDateTime => f.format(m)
      case m: DateTime => f.format(m)
      case m: java.sql.Timestamp => f.format(m)
      case m: Long => f.format(m)
      case m: String => format(style, DateTimeUtils.makeForFormatting(m))
    }
  }
}
