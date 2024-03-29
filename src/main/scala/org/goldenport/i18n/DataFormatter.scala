package org.goldenport.i18n

import org.joda.time._
import org.goldenport.RAISE
import org.goldenport.context.Consequence
import org.goldenport.context.DateTimeContext
import org.goldenport.values.PathName
import org.goldenport.value._
import org.goldenport.util.AnyUtils
import org.goldenport.util.DateUtils
import org.goldenport.util.TimeUtils
import org.goldenport.util.DateTimeUtils
import org.goldenport.util.DateFormatter
import org.goldenport.util.TimeFormatter
import org.goldenport.util.DateTimeFormatter

/*
 * @since   Dec.  9, 2022
 *  version Dec. 11, 2022
 *  version Feb. 20, 2023
 * @version Mar.  7, 2023
 * @author  ASAMI, Tomoharu
 */
case class DataFormatter(
  policy: DataFormatter.Policy,
  context: FormatterContext
) {
  import DataFormatter.{Directive, Format}

  def format(p: Any): String = policy.datatypes.lift(p).
    map(format(_, p)).
    getOrElse(DataFormatter.format(p))

  def format(directive: Option[Directive], p: Any): String =
    directive.map(format(_, p)).getOrElse(format(p))

  def format(directive: Directive, p: Any): String = {
    val f = policy.directives.lift(directive) getOrElse Format.ToString
    format(f, p)
  }

  def format(f: DataFormatter.Format, p: Any): String = f.format(p)(context)

  def formatPf(key: String, pf: PartialFunction[String, Any]): Option[String] = {
    val (k, d) = DataFormatter.toKeyDirective(key)
    pf.lift(k.v).map(x => format(d, x))
  }
}

object DataFormatter {
  object datatype {
    val standard: PartialFunction[Any, Directive] = {
      case m: Double => Directive.Double
      case m: BigDecimal => Directive.BigDecimal
      case m: DateTime => Directive.DateTime
      case m: LocalDateTime => Directive.DateTime
      case m: LocalDate => Directive.Date
      case m: LocalTime => Directive.Time
      case m: java.sql.Timestamp => Directive.DateTime
      case m: java.sql.Date => Directive.DateTime
      case m: java.util.Date => Directive.DateTime
    }
    val mysql: PartialFunction[Any, Directive] = {
      val a: PartialFunction[Any, Directive] = {
        case m: java.sql.Timestamp => Directive.DateTime
        case m: java.sql.Date => Directive.Date
        case m: java.util.Date => Directive.Date
      }
      a orElse standard
    }
  }

  case class Policy(
    directives: PartialFunction[Directive, Format],
    datatypes: PartialFunction[Any, Directive]
  ) {
    def withDirectives(p: PartialFunction[Directive, Format]) = copy(directives = p)
    def appendDirectives(p: PartialFunction[Directive, Format]) = copy(directives = directives orElse p)
    def prependDirectives(p: PartialFunction[Directive, Format]) = copy(directives = p orElse directives)
    def withDatatypes(p: PartialFunction[Any, Directive]) = copy(datatypes = p)
    def appendDatatyps(p: PartialFunction[Any, Directive]) = copy(datatypes = datatypes orElse p)
    def prependDatatyps(p: PartialFunction[Any, Directive]) = copy(datatypes = p orElse datatypes)

  }
  object Policy {
    private val _primitive_map = Map(
      Directive.Double -> DoubleFormat.Standard,
      Directive.BigDecimal -> BigDecimalFormat.Standard
    )

    val iso = Policy(
      Map(
        Directive.Date -> Format.Iso.Date,
        Directive.Time -> Format.Iso.Time,
        Directive.DateTime -> Format.Iso.DateTime
      ) ++ _primitive_map,
      datatype.standard
    )
    val web = Policy(
      Map(
        Directive.Date -> DateFormat.Web,
        Directive.Time -> TimeFormat.Web,
        Directive.DateTime -> DateTimeFormat.Web
      ) ++ _primitive_map,
      datatype.standard
    )
    val webNatural = Policy(
      Map(
        Directive.Date -> DateFormat.WebNatural,
        Directive.Time -> TimeFormat.WebNatural,
        Directive.DateTime -> DateTimeFormat.WebNatural
      ) ++ _primitive_map,
      datatype.standard
    )
    val application_ja = Policy(
      Map(
        Directive.Date -> Format.月日,
        Directive.Time -> Format.時分,
        Directive.DateTime -> Format.月日時分
      ) ++ _primitive_map,
      datatype.standard
    )

    def get(name: String): Option[Policy] = Option(name) collect {
      case "iso" => iso
      case "web" => web
      case "web-natural" => webNatural
      case "application-ja" => application_ja
    }
  }

  sealed trait Directive extends NamedValueInstance {
  }
  object Directive extends EnumerationClass[Directive] {
    val elements = Vector(Double, BigDecimal, Date, Time, DateTime)

    case object Double extends Directive {
      val name = "double"
    }
    case object BigDecimal extends Directive {
      val name = "bigdecimal"
    }
    case object Date extends Directive {
      val name = "date"
    }
    case object Time extends Directive {
      val name = "time"
    }
    case object DateTime extends Directive {
      val name = "datetime"
    }
    case class Unknown(name: String) extends Directive {
    }

    def resolve(p: String): Directive = get(p) getOrElse Unknown(p)
  }

  sealed trait Format extends NamedValueInstance {
    def format(p: Any)(implicit ctx: FormatterContext): String
  }
  sealed trait DateFormat extends Format {
    def format(p: Any)(implicit ctx: FormatterContext): String =
      DateFormatter.format(date_style, p)

    protected def date_style: DateFormatter.Style
  }
  object DateFormat {
    case object Web extends DateFormat {
      val name = "web"

      protected def date_style: DateFormatter.Style = DateFormatter.Style.Web
    }
    case object WebNatural extends DateFormat {
      val name = "web-natural"

      protected def date_style: DateFormatter.Style = DateFormatter.Style.WebNatural
    }

    def parse(p: String): Consequence[DateFormat] =
      DateFormatter.Style.get(p) match {
        case Some(s) => Consequence.success(Format.StyleDateFormat(s))
        case None => p match {
          case "web" => Consequence.success(Web)
          case "web-natural" => Consequence.success(WebNatural)
          case "月日" => Consequence.success(Format.月日)
          case m => Consequence.success(Format.StyleDateFormat(DateFormatter.Style.Pattern(m)))
        }
      }
  }
  sealed trait TimeFormat extends Format {
    def format(p: Any)(implicit ctx: FormatterContext): String =
      TimeFormatter.format(time_style, p)

    protected def time_style: TimeFormatter.Style
  }
  object TimeFormat {
    case object Web extends TimeFormat {
      val name = "web"

      protected def time_style: TimeFormatter.Style = TimeFormatter.Style.Web
    }
    case object WebNatural extends TimeFormat {
      val name = "web-natural"

      protected def time_style: TimeFormatter.Style = TimeFormatter.Style.WebNatural
    }

    def parse(p: String): Consequence[TimeFormat] =
      TimeFormatter.Style.get(p) match {
        case Some(s) => Consequence.success(Format.StyleTimeFormat(s))
        case None => p match {
          case "web" => Consequence.success(Web)
          case "web-natural" => Consequence.success(WebNatural)
          case "時分" => Consequence.success(Format.時分)
          case "時分秒" => Consequence.success(Format.時分秒)
          case m => Consequence.success(Format.StyleTimeFormat(TimeFormatter.Style.Pattern(m)))
        }
      }
  }
  sealed trait DateTimeFormat extends Format {
    def format(p: Any)(implicit ctx: FormatterContext): String =
      DateTimeFormatter.format(datetime_style, p)

    protected def datetime_style: DateTimeFormatter.Style
  }
  object DateTimeFormat {
    case object Web extends DateTimeFormat {
      val name = "web"

      protected def datetime_style: DateTimeFormatter.Style = DateTimeFormatter.Style.Web
    }
    case object WebNatural extends DateTimeFormat {
      val name = "web-natural"

      protected def datetime_style: DateTimeFormatter.Style = DateTimeFormatter.Style.WebNatural
    }

    def parse(p: String): Consequence[DateTimeFormat] =
      DateTimeFormatter.Style.get(p) match {
        case Some(s) => Consequence.success(Format.StyleDateTimeFormat(s))
        case None => p match {
          case "web" => Consequence.success(Web)
          case "web-natural" => Consequence.success(WebNatural)
          case "月日時分" => Consequence.success(Format.月日時分)
          case m => Consequence.success(Format.StyleDateTimeFormat(DateTimeFormatter.Style.Pattern(m)))
        }
      }
  }
  sealed trait DoubleFormat extends Format {
    def format(p: Any)(implicit ctx: FormatterContext): String
  }
  object DoubleFormat {
    case object Standard extends DoubleFormat {
      val name = "standard"

      def format(p: Any)(implicit ctx: FormatterContext): String = ctx.toString(p)
    }
  }

  sealed trait BigDecimalFormat extends Format {
    def format(p: Any)(implicit ctx: FormatterContext): String
  }
  object BigDecimalFormat {
    case object Standard extends BigDecimalFormat {
      val name = "standard"

      def format(p: Any)(implicit ctx: FormatterContext): String = ctx.toString(p)
    }
  }

  object Format extends EnumerationClass[Format] {
    val elements = Vector()

    case object ToString extends Format {
      val name = "string"

      def format(p: Any)(implicit ctx: FormatterContext): String = ctx.toString(p)
    }

    // See org.goldenport.util.DateFormatter
    case object 月日 extends DateFormat {
      val name = "月日"

      protected def date_style: DateFormatter.Style = DateFormatter.Style.月日
    }

    case class StyleDateFormat(style: DateFormatter.Style) extends DateFormat {
      def name = style.name

      protected def date_style: DateFormatter.Style = style
    }

    // See org.goldenport.util.TimeFormatter
    case object 時分 extends TimeFormat {
      val name = "時分"

      protected def time_style: TimeFormatter.Style = TimeFormatter.Style.時分
    }

    case object 時分秒 extends TimeFormat {
      val name = "時分秒"

      protected def time_style: TimeFormatter.Style = TimeFormatter.Style.時分秒
    }

    case class StyleTimeFormat(style: TimeFormatter.Style) extends TimeFormat {
      def name = style.name

      protected def time_style: TimeFormatter.Style = style
    }

    // See org.goldenport.util.DateTimeFormatter
    case object 月日時分 extends DateTimeFormat {
      val name = "月日時分"

      protected def datetime_style: DateTimeFormatter.Style = DateTimeFormatter.Style.月日時分
    }

    case class StyleDateTimeFormat(style: DateTimeFormatter.Style) extends DateTimeFormat {
      def name = style.name

      protected def datetime_style: DateTimeFormatter.Style = style
    }

    object Iso {
      case object Date extends DateFormat {
        val name = "iso.date"

        // def format(p: Any)(implicit ctx: FormatterContext): String = p match {
        //   case m: DateTime => DateUtils.toIsoTimeString(m, ctx.dateTimeZone)
        //   case m: java.sql.Timestamp => DateUtils.toIsoTimeString(m, ctx.dateTimeZone)
        //   case m: Long => DateUtils.toIsoTimeString(m, ctx.dateTimeZone)
        //   case m: String => DateUtils.toIsoTimeString(m, ctx.dateTimeZone)
        // }
      protected def date_style: DateFormatter.Style = DateFormatter.Style.Iso
      }

      case object Time extends TimeFormat {
        val name = "iso.time"

      protected def time_style: TimeFormatter.Style = TimeFormatter.Style.Iso
        // def format(p: Any)(implicit ctx: FormatterContext): String = p match {
        //   case m: DateTime => TimeUtils.toIsoTimeString(m, ctx.dateTimeZone)
        //   case m: java.sql.Timestamp => TimeUtils.toIsoTimeString(m, ctx.dateTimeZone)
        //   case m: Long => TimeUtils.toIsoTimeString(m, ctx.dateTimeZone)
        //   case m: String => TimeUtils.toIsoTimeString(m, ctx.dateTimeZone)
        // }
      }

      case object DateTime extends DateTimeFormat {
        val name = "iso.datetime"

        protected def datetime_style: DateTimeFormatter.Style = DateTimeFormatter.Style.Iso
        // def format(p: Any)(implicit ctx: FormatterContext): String = p match {
        //   case m: DateTime => DateTimeUtils.toIsoDateTimeString(m, ctx.dateTimeZone)
        //   case m: java.sql.Timestamp => DateTimeUtils.toIsoDateTimeString(m, ctx.dateTimeZone)
        //   case m: Long => DateTimeUtils.toIsoDateTimeString(m, ctx.dateTimeZone)
        //   case m: String => DateTimeUtils.toIsoDateTimeString(m, ctx.dateTimeZone)
        // }
      }
    }
  }

  def toKeyDirective(p: String): (PathName, Option[Directive]) = {
    val (k, a) = toKeyAttr(p)
    val d = a.map(Directive.resolve)
    (k, d)
  }

  private def toKeyAttr(p: String): (PathName, Option[String]) = {
    p.indexOf("__") match {
      case -1 => (PathName(p, "."), None)
      case n => (PathName(p.substring(0, n), "."), Some(p.substring(n + "__".length)))
    }
  }

  def format(p: Any): String = AnyUtils.toString(p)

  def format(directive: Option[Directive], p: Any): String =
    directive.map(format(_, p)).getOrElse(format(p))

  def format(directive: Directive, p: Any): String = {
    val f = Policy.iso.directives.lift(directive) getOrElse Format.ToString
    format(f, p)
  }

  def format(f: DataFormatter.Format, p: Any): String = f.format(p)(FormatterContext.now)
}
