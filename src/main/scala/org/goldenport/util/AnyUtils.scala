package org.goldenport.util

import scala.util.control.NonFatal
import scala.concurrent.duration._
import scala.xml.NodeSeq
import java.util.Date
import java.sql.Timestamp
import java.io.File
import java.net.{URL, URI}
import java.util.concurrent.TimeUnit
import org.joda.time._
import com.asamioffice.goldenport.io.UURL
import org.goldenport.RAISE
import org.goldenport.extension.Showable

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
 * @version Apr. 20, 2021
 * @author  ASAMI, Tomoharu
 */
object AnyUtils {
  def toString(x: Any): String = {
    x match {
      case v: Timestamp => DateTimeUtils.toIsoDateTimeStringJst(v)
      case v: Symbol => v.name
      case m: NodeSeq => m.toString
      case m: Seq[_] => m.map(toEmbed(_)).mkString(",")
      case m: Array[_] => m.map(toEmbed(_)).mkString(",")
      case m: Showable => m.print
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
  def toEmbed(x: Any): String = x match {
    case m: Showable => m.embed
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
    }
  }
  def toDate(x: Any): Date = {
    x match {
      case v: Date => v
      case v: Long => new Date(v)
      case s: String => DateUtils.parse(s)
    }
  }
  def toDateTime(x: Any): DateTime = RAISE.notImplementedYetDefect
  def toLocalDate(x: Any): LocalDate = RAISE.notImplementedYetDefect
  def toLocalTime(x: Any): LocalTime = {
    x match {
      case v: LocalTime => v
      case s: String => LocalTime.parse(s)
    }
  }
  def toFiniteDuration(x: Any): FiniteDuration = x match {
    case m: FiniteDuration => m
    case m: Long => FiniteDuration(m, TimeUnit.MILLISECONDS)
    case m: String => toFiniteDuration(m.toLong) // TODO
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
}
