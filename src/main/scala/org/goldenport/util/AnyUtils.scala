package org.goldenport.util

import scala.concurrent.duration._
import scala.xml.NodeSeq
import java.util.Date
import java.sql.Timestamp
import java.io.File
import java.net.{URL, URI}
import java.util.concurrent.TimeUnit
import com.asamioffice.goldenport.io.UURL

/*
 * See org.goldenport.record.util.AnyUtils
 * 
 * @since   Jun. 10, 2014
 *  version Jan. 28, 2015
 *  version Apr. 29, 2016
 *  version Apr. 27, 2017
 *  version Aug. 29, 2017
 *  version Sep.  4, 2017
 * @version Oct. 31, 2017
 * @author  ASAMI, Tomoharu
 */
object AnyUtils {
  def toString(x: Any): String = {
    x match {
      case v: Timestamp => DateTimeUtils.toIsoDateTimeStringJst(v)
      case v: Symbol => v.name
      case m: NodeSeq => m.toString
      case m: Seq[_] => m.map(toString(_)).mkString(",")
      case m: Array[_] => m.map(toString(_)).mkString(",")
      case _ => x.toString
    }
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
      case v => toString(v).toByte
    }
  }
  def toShort(x: Any): Short = {
    x match {
      case v: Byte => v
      case v: Short => v
      case v => toString(v).toShort
    }
  }
  def toInt(x: Any): Int = {
    x match {
      case v: Byte => v
      case v: Short => v
      case v: Int => v
      case v => toString(v).toInt
    }
  }
  def toLong(x: Any): Long = {
    x match {
      case v: Byte => v
      case v: Short => v
      case v: Int => v
      case v: Long => v
      case v => toString(v).toLong
    }
  }
  def toFloat(x: Any): Float = {
    x match {
      case v: Float => v
      case v => toString(v).toFloat
    }
  }
  def toDouble(x: Any): Double = {
    x match {
      case v: Float => v
      case v: Double => v
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
      case v => BigDecimal(toString(v))
    }
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
}
