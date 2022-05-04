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

/*
 * @since   May.  1, 2022
 * @version May.  3, 2022
 * @author  ASAMI, Tomoharu
 */
trait DateTimeFormatter {
  def format(p: DateTime): String
  def format(p: Long): String
}

trait JavaDateTimeFormatter extends DateTimeFormatter with DateTimeFormatter.JavaFormatter {
}

object DateTimeFormatter {
  trait JavaFormatter {
    def tz: TimeZone

    private def _sdf(p: DateTimeZone): DateFormat = java_Sdf(DateTimeUtils.dateTimeZoneToTz(p))

    private def _sdf_tz = java_Sdf(tz)

    // thread unsafe
    protected def java_Sdf(p: TimeZone): DateFormat

    def format(p: DateTime) = _sdf(p.getZone).format(p.getMillis)
    def format(p: Long) = _sdf_tz.format(p)
  }

  trait JodaFormatterBase {
    def tz: TimeZone

    private def _sdf(p: DateTimeZone): JodaDateTimeFormatter = joda_Sdf(DateTimeUtils.dateTimeZoneToTz(p))

    private def _sdf_tz = joda_Sdf(tz)

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
    dateStyle: Int,
    timeStyle: Int
  ) extends JavaDateTimeFormatter {
    protected def java_Sdf(p: TimeZone) = {
      val sdf = DateFormat.getDateTimeInstance(dateStyle, timeStyle, locale)
      sdf.setTimeZone(p)
      sdf
    }
  }

  case class PatternJavaFormatter(
    locale: Locale,
    tz: TimeZone,
    pattern: String
  ) extends JavaDateTimeFormatter {
    protected def java_Sdf(p: TimeZone) = {
      val sdf = new SimpleDateFormat(pattern)
      sdf.setTimeZone(p)
      sdf
    }
  }

  case class DateFormatFormatter(format: DateFormat) extends DateTimeFormatter {
    def format(p: DateTime) = format.format(p.getMillis)
    def format(p: Long) = format.format(p)
  }

  case class JodaFormatter(
    locale: Locale,
    tz: TimeZone,
    formatter: JodaDateTimeFormatter
  ) extends DateTimeFormatter with JodaFormatterBase {
    protected def joda_Sdf(p: TimeZone) = formatter.withZone(DateTimeUtils.tzToDateTimeZone(p))
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
      PatternJavaFormatter(locale, tz, "M月d日H時m分")
    else
      NamedJavaFormatter(locale, tz, DateFormat.MEDIUM, DateFormat.MEDIUM)

  private def _natural_year(
    locale: Locale,
    tz: TimeZone
  ) =
    if (LocaleUtils.isJapanese(locale))
      PatternJavaFormatter(locale, tz, "yyyy年M月d日H時m分")
    else
      NamedJavaFormatter(locale, tz, DateFormat.MEDIUM, DateFormat.MEDIUM)
}
