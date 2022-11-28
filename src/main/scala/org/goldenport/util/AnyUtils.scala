package org.goldenport.util

import scala.util.control.NonFatal
import scala.concurrent.duration._
import scala.xml.NodeSeq
import java.util.Date
import java.util.TimeZone
import java.sql.Timestamp
import java.io.File
import java.net.{URL, URI}
import play.api.libs.json.JsValue
import java.util.concurrent.TimeUnit
import org.joda.time.{Duration => _, _}
import com.asamioffice.goldenport.io.UURL
import org.goldenport.RAISE
import org.goldenport.extension.Showable
import org.goldenport.extension.IRecord
import org.goldenport.context.Consequence
import org.goldenport.i18n.StringFormatter
import org.goldenport.values.DateTimePeriod

/*
 * See org.goldenport.record.util.AnyUtils
 * 
 * @since   Jun. 10, 2014
 *  version Jan. 28, 2015
 *  version Apr. 29, 2016
 *  version Apr. 27, 2017
 *  version Aug. 29, 2017
 *  version Sep.  4, 2017
 *  version Oct. 31, 2017
 *  version Nov. 13, 2017
 *  version Dec. 17, 2017
 *  version Sep. 23, 2019
 *  version Oct.  8, 2019
 *  version Nov. 28, 2019
 *  version Jan. 18, 2020
 *  version Jan. 23, 2021
 *  version Apr. 20, 2021
 *  version Nov.  5, 2021
 *  version Jan. 30, 2022
 *  version Feb. 24, 2022
 *  version Mar.  9, 2022
 *  version May.  3, 2022
 *  version Sep. 27, 2022
 *  version Oct. 29, 2022
 * @version Nov. 28, 2022
 * @author  ASAMI, Tomoharu
 */
object AnyUtils {
  def toString(x: Any): String = {
    x match {
      case m: String => m
      case v: Timestamp => DateTimeUtils.toIsoDateTimeStringJst(v)
      case v: Symbol => v.name
      case m: NodeSeq => m.toString
      case m: Seq[_] => m.map(toString(_)).mkString(",")
      case m: Array[_] => m.map(toString(_)).mkString(",")
      case m: Showable => m.print
      case m: JsValue => m.toString
      case m: MonthDay => f"${m.getMonthOfYear}%02d-${m.getDayOfMonth}%02d"
      case _ => x.toString
    }
  }
  def toPrint(x: Any): String = x match {
    case m: Showable => m.print
    case m => toString(m)
  }
  def toDisplay(x: Any): String = x match {
    case m: Showable => m.display
    case m => toString(m)
  }
  def toShow(x: Any): String = x match {
    case m: Showable => m.show
    case m => toString(m)
  }
  def toEmbed(x: Any): String = toEmbed(x, 8)
  def toEmbed(x: Any, width: Int): String = x match {
    case m: Showable => m.embed(width)
    case m => toEmbedString(toString(m), width)
  }
  def toEmbedCenter(x: Any, width: Int): String =
    StringFormatter.display.enCenter(toEmbed(x, width), width)
  def toEmbedRight(x: Any, width: Int): String =
    StringFormatter.display.enRight(toEmbed(x, width), width)

  def toEmbedString(p: String, width: Int): String = StringUtils.toEmbedConsole(p, width)

  def toMarshall(p: Any): String = p match {
    case m: Showable => m.print
    case m => toString(m)
  }

  def toBoolean(x: Any): Boolean = {
    x match {
      case v: Boolean => v
      case v => toString(v).toLowerCase match {
        case "1" => true
        case "0" => false
        case "true" => true
        case "false" => false
      }
    }
  }
  def toByte(x: Any): Byte = {
    x match {
      case v: Byte => v
      case v: Number => v.byteValue // TODO validation
      case v => toString(v).toByte
    }
  }
  def toShort(x: Any): Short = {
    x match {
      case v: Byte => v
      case v: Short => v
      case v: Number => v.shortValue // TODO validation
      case v => toString(v).toShort
    }
  }
  def toInt(x: Any): Int = {
    x match {
      case v: Byte => v
      case v: Short => v
      case v: Int => v
      case v: Number => v.intValue // TODO validation
      case v => toString(v).toInt
    }
  }
  def toLong(x: Any): Long = {
    x match {
      case v: Byte => v
      case v: Short => v
      case v: Int => v
      case v: Long => v
      case v: Number => v.longValue // TODO validation
      case m: FiniteDuration => m.toMillis
      case m: DateTime => m.getMillis
      case v => toString(v).toLong
    }
  }
  def toFloat(x: Any): Float = {
    x match {
      case v: Float => v
      case v: Number => v.floatValue // TODO validation
      case v => toString(v).toFloat
    }
  }
  def toDouble(x: Any): Double = {
    x match {
      case v: Float => v
      case v: Double => v
      case v: Number => v.doubleValue // TODO validation
      case v => toString(v).toDouble
    }
  }
  def toBigInt(x: Any): BigInt = {
    x match {
      case v: Byte => BigInt(v)
      case v: Short => BigInt(v)
      case v: Int => BigInt(v)
      case v: Long => BigInt(v)
      case v: BigInt => v
//      case v: Number => BigInt(v)
      case v => BigInt(toString(v))
    }
  }
  def toBigDecimal(x: Any): BigDecimal = {
    x match {
      case v: Byte => BigDecimal(v)
      case v: Short => BigDecimal(v)
      case v: Int => BigDecimal(v)
      case v: Long => BigDecimal(v)
      case v: Float => BigDecimal(v)
      case v: Double => BigDecimal(v)
      case v: BigInt => BigDecimal(v)
      case v: BigDecimal => v
//      case v: Number => BigDecimal(v)
      case v => BigDecimal(toString(v))
    }
  }
  def toNumber(x: Any): Number = x match {
    case m: BigInt => m
    case m: BigDecimal => m
    case m: Number => m
    case m: Byte => m
    case m: Short => m
    case m: Int => m
    case m: Long => m
    case m: Float => m
    case m: Double => m
    case m: String => StringUtils.numberOption(m).
        getOrElse(RAISE.invalidArgumentFault(s"Invalid number: $m"))
  }
  def toSpireNumber(x: Any): spire.math.Number = x match {
    case m: BigInt => spire.math.Number(m)
    case m: BigDecimal => spire.math.Number(m)
    case m: Byte => spire.math.Number(m)
    case m: Short => spire.math.Number(m)
    case m: Int => spire.math.Number(m)
    case m: Long => spire.math.Number(m)
    case m: Float => spire.math.Number(m)
    case m: Double => spire.math.Number(m)
    case m: Number => spire.math.Number(toBigDecimal(m))
    case m: String => spire.math.Number(m)
  }
  def toTimestamp(x: Any): Timestamp = {
    x match {
      case v: Timestamp => v
      case v: Long => new Timestamp(v)
      case s: String => TimestampUtils.parse(s)
      case m: DateTime => TimestampUtils.toTimestamp(m)
    }
  }
  def toDate(x: Any): Date = {
    x match {
      case v: Date => v
      case v: Long => new Date(v)
      case s: String => DateUtils.parse(s)
    }
  }
  def toDateTime(x: Any): DateTime = x match {
    case m: DateTime => m
  }
  def toLocalDate(x: Any): LocalDate = x match {
    case m: LocalDate => m
  }
  def toLocalTime(x: Any): LocalTime = {
    x match {
      case v: LocalTime => v
      case s: String => LocalTime.parse(s)
    }
  }
  def toUnixTime(tz: TimeZone, x: Any): Long = x match {
    case m: Long => m
    case m: DateTime => m.getMillis
  }
  def toFiniteDuration(x: Any): FiniteDuration = x match {
    case m: FiniteDuration => m
    case m: Int => FiniteDuration(m, TimeUnit.MILLISECONDS)
    case m: Long => FiniteDuration(m, TimeUnit.MILLISECONDS)
    case m: String => NumberUtils.getLong(m).
        map(toFiniteDuration).
        getOrElse(Duration.create(m).asInstanceOf[FiniteDuration])
  }
  def toFiniteDurationMinute(x: Any): FiniteDuration = x match {
    case m: FiniteDuration => m
    case m: Int => FiniteDuration(m, TimeUnit.MINUTES)
    case m: Long => FiniteDuration(m, TimeUnit.MINUTES)
    case m: String => NumberUtils.getLong(m).
        map(toFiniteDurationMinute).
        getOrElse(Duration.create(m).asInstanceOf[FiniteDuration])
  }
  def toUrl(x: Any): URL = {
    x match {
      case m: URL => m
      case m: URI => m.toURL
      case m: File => m.toURI.toURL
      case s: String => UURL.getURLFromFileOrURLName(s)
    }
  }
  def toUri(x: Any): URI = {
    x match {
      case m: URL => m.toURI
      case m: URI => m
      case m: File => m.toURI
      case s: String => new URI(s)
    }
  }
  def toRecord(x: Any): IRecord = {
    x match {
      case m: DateTimePeriod => m.toRecord
      case m: FiniteDuration => DurationUtils.toRecord(m)
    }
  }

  def isNumber(p: Any): Boolean = p match {
    case _: Boolean => true
    case _: Byte => true
    case _: Short => true
    case _: Int => true
    case _: Long => true
    case _: Float => true
    case _: Double => true
    case _: BigInt => true
    case _: BigDecimal => true
    case _: java.math.BigInteger => true
    case _: java.math.BigDecimal => true
    case _: Number => true
    case _ => false
  }

  def guessNumber(p: Any): Boolean = p match {
    case m: String => StringUtils.isNumber(m)
    case _ => isNumber(p)
  }

  def guessNumberOrEmpty(p: Any): Boolean = p match {
    case "" => true
    case _ => guessNumber(p)
  }

  def getByte(p: Any): Option[Byte] = NumberUtils.optionByte(p)
  def getShort(p: Any): Option[Short] = NumberUtils.optionShort(p)
  def getInt(p: Any): Option[Int] = NumberUtils.optionInt(p)
  def getLong(p: Any): Option[Long] = NumberUtils.optionLong(p)
  def getFloat(p: Any): Option[Float] = NumberUtils.optionFloat(p)
  def getDouble(p: Any): Option[Double] = NumberUtils.optionDouble(p)
  def getBigInt(p: Any): Option[BigInt] = NumberUtils.optionBigInt(p)
  def getBigDecimal(p: Any): Option[BigDecimal] = NumberUtils.optionBigDecimal(p)

  def consequenceBoolean(p: Any): Consequence[Boolean] = Consequence(toBoolean(p))
  def consequenceShort(p: Any): Consequence[Short] = Consequence(toShort(p))
  def consequenceInt(p: Any): Consequence[Int] = Consequence(toInt(p))
  def consequenceLong(p: Any): Consequence[Long] = Consequence(toLong(p))
  def consequenceFloat(p: Any): Consequence[Float] = Consequence(toFloat(p))
  def consequenceDouble(p: Any): Consequence[Double] = Consequence(toDouble(p))
  def consequenceBigDecimal(p: Any): Consequence[BigDecimal] = Consequence(toBigDecimal(p))
  def consequenceString(p: Any): Consequence[String] = Consequence(toString(p))
  def consequenceUrl(p: Any): Consequence[URL] = Consequence(toUrl(p))
  def consequenceDateTime(p: Any): Consequence[DateTime] = Consequence(toDateTime(p))
  def consequenceDuration(p: Any): Consequence[Duration] = p match {
    case m: String => Consequence(Duration.create(m))
    case m: Long => Consequence.success(Duration.create(m, MILLISECONDS))
    case m: Int => Consequence.success(Duration.create(m, MILLISECONDS))
    case m => Consequence.valueDomainFault(toString(m))
  }
  def consequenceFiniteDuration(p: Any): Consequence[FiniteDuration] =
    consequenceDuration(p) flatMap {
      case m: FiniteDuration => Consequence.success(m)
      case m => Consequence.valueDomainFault(toString(toString(m)))
    }
}
