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

/*
 * @since   May.  2, 2022
 * @version May.  3, 2022
 * @author  ASAMI, Tomoharu
 */
trait DateFormatter {
  def format(p: DateTime): String
  def format(p: Long): String
}

trait JavaDateFormatter extends DateFormatter with DateTimeFormatter.JavaFormatter {
}

object DateFormatter {
  sealed trait Style extends NamedValueInstance {
  }
  sealed trait JavaStyle extends Style {
  }
  sealed trait JodaStyle extends Style {
  }
  object Style extends EnumerationClass[Style] {
    val elements = Vector(
      Full, Long, Medium, Short,
      Iso, IsoBasic, IsoBasicOrdinal,
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
    case object IsoBasic extends JodaStyle {
      val name = "iso-basic"
    }
    case object IsoBasicOrdinal extends JodaStyle {
      val name = "iso-basic-ordinal"
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
    locale: Locale,
    tz: TimeZone,
    dateStyle: Int
  ) extends JavaDateFormatter {
    protected final def java_Sdf(p: TimeZone): DateFormat = {
      val sdf = DateFormat.getDateInstance(dateStyle, locale)
      sdf.setTimeZone(p)
      sdf
    }
  }

  case class PatternJavaFormatter(
    locale: Locale,
    tz: TimeZone,
    pattern: String
  ) extends JavaDateFormatter {
    protected def java_Sdf(p: TimeZone): DateFormat = {
      val sdf = new SimpleDateFormat(pattern)
      sdf.setTimeZone(p)
      sdf
    }
  }

  case class DateFormatFormatter(format: DateFormat) extends DateFormatter {
    def format(p: DateTime) = format.format(p.getMillis)
    def format(p: Long) = format.format(p)
  }

  case class JodaFormatter(
    locale: Locale,
    tz: TimeZone,
    formatter: JodaDateTimeFormatter
  ) extends DateFormatter {
    // thread safe
    private val _formatter = formatter.withZone(DateTimeUtils.tzToDateTimeZone(tz))
    private def _formatter(p: DateTimeZone) = formatter.withZone(p)

    def format(p: DateTime) = _formatter(p.getZone).print(p.getMillis)
    def format(p: Long) = _formatter.print(p)
  }

  val default = _natural(Locale.ENGLISH, DateTimeUtils.gmt)

  def create(
    locale: Locale,
    tz: DateTimeZone
  ): DateFormatter = create(locale, DateTimeUtils.dateTimeZoneToTz(tz))

  def create(
    locale: Locale,
    tz: TimeZone
  ): DateFormatter = create(locale, tz, Style.Natural)

  def create(
    locale: Locale,
    tz: TimeZone,
    name: String
  ): DateFormatter = {
    val style = Style(name)
    create(locale, tz, style)
  }

  def create(
    locale: Locale,
    tz: TimeZone,
    style: Style
  ): DateFormatter = {
    style match {
      case Style.Full => NamedJavaFormatter(locale, tz, DateFormat.FULL)
      case Style.Long => NamedJavaFormatter(locale, tz, DateFormat.LONG)
      case Style.Medium => NamedJavaFormatter(locale, tz, DateFormat.MEDIUM)
      case Style.Short => NamedJavaFormatter(locale, tz, DateFormat.SHORT)
      case Style.Iso => JodaFormatter(locale, tz, ISODateTimeFormat.date)
      case Style.IsoBasic => JodaFormatter(locale, tz, ISODateTimeFormat.basicDate)
      case Style.IsoBasicOrdinal => JodaFormatter(locale, tz, ISODateTimeFormat.basicOrdinalDate)
      case Style.Simple => PatternJavaFormatter(locale, tz, "yyyyMMdd")
      case Style.Natural => _natural(locale, tz)
      case Style.Pattern(pattern) => PatternJavaFormatter(locale, tz, pattern)
    }
  }

  private def _natural(
    locale: Locale,
    tz: TimeZone
  ) =
    if (LocaleUtils.isJapanese(locale))
      PatternJavaFormatter(locale, tz, "yyyy年M月d日")
    else
      NamedJavaFormatter(locale, tz, DateFormat.MEDIUM)
}
