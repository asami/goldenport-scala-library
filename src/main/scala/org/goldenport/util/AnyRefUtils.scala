package org.goldenport.util

import java.util.Date
import java.sql.Timestamp

/*
 * @since   Jun. 11, 2014
 *  version Apr. 29, 2016
 *  version Feb. 27, 2017
 * @version Aug. 29, 2017
 * @author  ASAMI, Tomoharu
 */
object AnyRefUtils {
  def toString(x: Any): String = AnyUtils.toString(x)
  def toBoolean(x: Any): java.lang.Boolean = AnyUtils.toBoolean(x)
  def toByte(x: Any): java.lang.Byte = AnyUtils.toByte(x)
  def toShort(x: Any): java.lang.Short = AnyUtils.toShort(x)
  def toInt(x: Any): java.lang.Integer = AnyUtils.toInt(x)
  def toLong(x: Any): java.lang.Long = AnyUtils.toLong(x)
  def toFloat(x: Any): java.lang.Float = AnyUtils.toFloat(x)
  def toDouble(x: Any): java.lang.Double = AnyUtils.toDouble(x)
  def toBigInt(x: Any): BigInt = AnyUtils.toBigInt(x)
  def toBigDecimal(x: Any): BigDecimal = AnyUtils.toBigDecimal(x)
  def toTimestamp(x: Any): Timestamp = AnyUtils.toTimestamp(x)
  def toDate(x: Any): Date = AnyUtils.toDate(x)
  def toAnyRef(x: Any): AnyRef = x match {
    case x: Boolean => x: java.lang.Boolean
    case x: Byte => x: java.lang.Byte
    case x: Short => x: java.lang.Short
    case x: Int => x: java.lang.Integer
    case x: Long => x: java.lang.Long
    case x: Float => x: java.lang.Float
    case x: Double => x: java.lang.Double
    case _ => x.asInstanceOf[AnyRef]
  }
}
