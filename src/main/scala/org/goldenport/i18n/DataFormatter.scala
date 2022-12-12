package org.goldenport.i18n

import org.joda.time._
import org.goldenport.RAISE
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
 * @version Dec. 11, 2022
 * @author  ASAMI, Tomoharu
 */
case class DataFormatter(
  policy: DataFormatter.Policy,
  context: FormatterContext
) {
  import DataFormatter.{Directive, Format}

  def format(p: Any): String = DataFormatter.format(p)

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
      case m: DateTime => Directive.DateTime
      case m: java.sql.Timestamp => Directive.DateTime
      case m: java.sql.Date => Directive.DateTime
      case m: java.util.Date => Directive.DateTime
    }
    val mysql: PartialFunction[Any, Directive] = {
      case m: DateTime => Directive.DateTime
      case m: java.sql.Timestamp => Directive.DateTime
      case m: java.sql.Date => Directive.Date
      case m: java.util.Date => Directive.Date
    }
  }

  case class Policy(
    directives: PartialFunction[Directive, Format],
    datatypes: PartialFunction[Any, Directive]
  )
  object Policy {
    val iso = Policy(
      Map(
        Directive.Date -> Format.Iso.Date,
        Directive.Time -> Format.Iso.Time,
        Directive.DateTime -> Format.Iso.DateTime
      ),
      datatype.standard
    )
    val application_ja = Policy(
      Map(
        Directive.Date -> Format.月日,
        Directive.Time -> Format.時分,
        Directive.DateTime -> Format.月日時分
      ),
      datatype.standard
    )
  }

  sealed trait Directive extends NamedValueInstance {
  }
  object Directive extends EnumerationClass[Directive] {
    val elements = Vector(Date, Time, DateTime)

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
  sealed trait TimeFormat extends Format {
    def format(p: Any)(implicit ctx: FormatterContext): String =
      TimeFormatter.format(time_style, p)

    protected def time_style: TimeFormatter.Style
  }
  sealed trait DateTimeFormat extends Format {
    def format(p: Any)(implicit ctx: FormatterContext): String =
      DateTimeFormatter.format(datetime_style, p)

    protected def datetime_style: DateTimeFormatter.Style
  }

  object Format extends EnumerationClass[Format] {
    val elements = Vector()

    case object ToString extends Format {
      val name = "string"

      def format(p: Any)(implicit ctx: FormatterContext): String = AnyUtils.toString(p)
    }

    // See org.goldenport.util.DateFormatter
    case object 月日 extends DateFormat {
      val name = "月日"

      protected def date_style: DateFormatter.Style = DateFormatter.Style.月日
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

    // See org.goldenport.util.DateTimeFormatter
    case object 月日時分 extends DateTimeFormat {
      val name = "月日時分"

      protected def datetime_style: DateTimeFormatter.Style = DateTimeFormatter.Style.月日時分
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
