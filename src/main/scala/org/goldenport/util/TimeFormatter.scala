package org.goldenport.util

import java.util.{Locale, TimeZone}
import java.text.DateFormat
import java.text.SimpleDateFormat
import org.joda.time._
import org.joda.time.format.{DateTimeFormatter => JodaDateTimeFormatter}
import org.joda.time.format.ISODateTimeFormat
import org.goldenport.parser._
import org.goldenport.value._
import org.goldenport.i18n.LocaleUtils
import org.goldenport.i18n.FormatterContext

/*
 * @since   May.  2, 2022
 *  version May.  3, 2022
 *  version Dec. 10, 2022
 *  version Feb. 17, 2023
 * @version Mar.  7, 2023
 * @author  ASAMI, Tomoharu
 */
trait TimeFormatter {
  def format(p: LocalTime): String
  def format(p: DateTime): String
  def format(p: java.util.Date): String = format(p.getTime)
  def format(p: java.sql.Date): String
  def format(p: java.sql.Timestamp): String = format(p.getTime)
  def format(p: Long): String
}

trait JavaTimeFormatter extends TimeFormatter with DateTimeFormatter.JavaFormatter {
  def format(p: LocalTime): String = format(p.toDateTimeToday(dateTimeZone))
  def format(p: java.sql.Date): String = format(new DateTime(p.getTime, effectiveDatabaseDateTimeZone))
}

object TimeFormatter {
  sealed trait Style extends NamedValueInstance {
  }
  sealed trait JavaStyle extends Style {
  }
  sealed trait JodaStyle extends Style {
  }
  object Style extends EnumerationClass[Style] {
    val elements = Vector(
      Full, Long, Medium, Short,
      Iso, IsoFull, IsoBasic, IsoBasicFull,
      Simple, Natural
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
    case object Simple extends Style {
      val name = "simple"
    }
    case object Natural extends Style {
      val name = "natural"
    }
    case object Web extends Style {
      val name = "web"
    }
    case object WebNatural extends Style {
      val name = "web-natural"
    }
    case class Pattern(pattern: String) extends Style {
      val name = "pattern"
    }
    val 時分 = Pattern("H時m分")
    val 時分秒 = Pattern("H時m分s秒")

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
    timeStyle: Int
  ) extends JavaTimeFormatter {
    def locale = context.locale
    def tz = context.timezone

    protected def java_Sdf(p: TimeZone) = {
      val sdf = DateFormat.getTimeInstance(timeStyle, locale)
      sdf.setTimeZone(p)
      sdf
    }
  }
  object NamedJavaFormatter {
    def apply(l: Locale, tz: TimeZone, dateStyle: Int): NamedJavaFormatter =
      NamedJavaFormatter(FormatterContext(l, tz), dateStyle)
  }

  case class PatternJavaFormatter(
    context: FormatterContext,
    pattern: String
  ) extends JavaTimeFormatter {
    def tz = context.timezone

    protected def java_Sdf(p: TimeZone) = {
      val sdf = new SimpleDateFormat(pattern)
      sdf.setTimeZone(p)
      sdf
    }
  }
  object PatternJavaFormatter {
    def apply(l: Locale, tz: TimeZone, pattern: String): PatternJavaFormatter =
      PatternJavaFormatter(FormatterContext(l, tz), pattern)
  }

  case class DateFormatFormatter(format: DateFormat) extends TimeFormatter {
    def format(p: LocalTime): String = format.format(p)
    def format(p: DateTime) = format.format(p.getMillis) // ??? as-is
    def format(p: java.sql.Date): String = format.format(p)
    def format(p: Long) = format.format(p)
  }

  case class JodaFormatter(
    context: FormatterContext,
    formatter: JodaDateTimeFormatter
  ) extends TimeFormatter {
    def tz = context.timezone
    def databaseTimeZone = context.effectiveDatabaseTimeZone

    private lazy val _dtz = DateTimeUtils.tzToDateTimeZone(tz)
    private lazy val _db_dtz = DateTimeUtils.tzToDateTimeZone(databaseTimeZone)

    // thread safe
    private val _formatter = formatter.withZone(DateTimeUtils.tzToDateTimeZone(tz))
    private def _formatter(p: DateTimeZone) = formatter.withZone(p)

    def format(p: LocalTime): String = format(p.toDateTimeToday(_dtz))
    def format(p: DateTime) = _formatter(p.getZone).print(p.getMillis)
    def format(p: java.sql.Date): String = format(new LocalTime(p.getTime, _db_dtz))
    def format(p: Long) = _formatter.print(p)
  }
  object JodaFormatter {
    def apply(l: Locale, tz: TimeZone, formatter: JodaDateTimeFormatter): JodaFormatter =
      JodaFormatter(FormatterContext(l, tz), formatter)
  }

  val default = _natural(Locale.ENGLISH, DateTimeUtils.gmt)

  def create(
    locale: Locale,
    tz: DateTimeZone
  ): TimeFormatter = create(locale, DateTimeUtils.dateTimeZoneToTz(tz))

  def create(
    locale: Locale,
    tz: TimeZone
  ): TimeFormatter = create(locale, tz, Style.Natural)

  def create(
    locale: Locale,
    tz: TimeZone,
    name: String
  ): TimeFormatter = {
    val style = Style(name)
    create(locale, tz, style)
  }

  def create(
    locale: Locale,
    tz: TimeZone,
    style: Style
  ): TimeFormatter = {
    style match {
      case Style.Full => NamedJavaFormatter(locale, tz, DateFormat.FULL)
      case Style.Long => NamedJavaFormatter(locale, tz, DateFormat.LONG)
      case Style.Medium => NamedJavaFormatter(locale, tz, DateFormat.MEDIUM)
      case Style.Short => NamedJavaFormatter(locale, tz, DateFormat.SHORT)
      case Style.Iso => JodaFormatter(locale, tz, ISODateTimeFormat.timeNoMillis)
      case Style.IsoFull => JodaFormatter(locale, tz, ISODateTimeFormat.time)
      case Style.IsoBasic => JodaFormatter(locale, tz, ISODateTimeFormat.basicTimeNoMillis)
      case Style.IsoBasicFull => JodaFormatter(locale, tz, ISODateTimeFormat.basicTime)
      case Style.Web => PatternJavaFormatter(locale, tz, "HH:mm:ss")
      case Style.WebNatural => PatternJavaFormatter(locale, tz, "HH:mm")
      case Style.Simple => PatternJavaFormatter(locale, tz, "HHmmss")
      case Style.Natural => _natural(locale, tz)
      case Style.Pattern(pattern) => PatternJavaFormatter(locale, tz, pattern)
    }
  }

  private def _natural(
    locale: Locale,
    tz: TimeZone
  ) =
    if (LocaleUtils.isJapanese(locale))
      PatternJavaFormatter(locale, tz, "H時m分")
    else
      NamedJavaFormatter(locale, tz, DateFormat.MEDIUM)

  def format(
    style: Style,
    p: Any
  )(implicit ctx: FormatterContext): String = {
    val f = create(ctx.locale, ctx.timezone, style)
    p match {
      case m: LocalTime => f.format(m)
      case m: DateTime => f.format(m)
      case m: java.sql.Timestamp => f.format(m)
      case m: java.sql.Date => f.format(m)
      case m: java.util.Date => f.format(m)
      case m: Long => f.format(m)
      case m: String => format(style, TimeUtils.makeForFormatting(m))
    }
  }
}
