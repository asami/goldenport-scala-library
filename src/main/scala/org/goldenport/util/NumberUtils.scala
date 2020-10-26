package org.goldenport.util

import scala.util.control.NonFatal
import org.goldenport.Strings
import org.goldenport.parser.ParseResult

/*
 * @since   Mar. 12, 2020
 *  version Apr. 21, 2020
 *  version Sep. 29, 2020
 * @version Oct. 20, 2020
 * @author  ASAMI, Tomoharu
 */
object NumberUtils {
  def getBoolean(p: Option[String]): Option[Boolean] = p.flatMap(getBoolean)

  def getBoolean(p: String): Option[Boolean] = p.trim.toLowerCase match {
    case "1" => Some(true)
    case "0" => Some(false)
    case "true" => Some(true)
    case "false" => Some(false)
    case _ => None
  }

  def getByte(p: Option[String]): Option[Byte] = p.flatMap(getByte)

  def getByte(p: String): Option[Byte] = try {
    if (Strings.blankp(p))
      None
    else
      Some(p.trim.toByte)
  } catch {
    case e: NumberFormatException => None
  }

  def getShort(p: Option[String]): Option[Short] = p.flatMap(getShort)

  def getShort(p: String): Option[Short] = try {
    if (Strings.blankp(p))
      None
    else
      Some(p.trim.toShort)
  } catch {
    case e: NumberFormatException => None
  }

  def getInt(p: Option[String]): Option[Int] = p.flatMap(getInt)

  def getInt(p: String): Option[Int] = try {
    if (Strings.blankp(p))
      None
    else
      Some(p.trim.toInt)
  } catch {
    case e: NumberFormatException => None
  }

  def getLong(p: Option[String]): Option[Long] = p.flatMap(getLong)

  def getLong(p: String): Option[Long] = try {
    if (Strings.blankp(p))
      None
    else
      Some(p.trim.toLong)
  } catch {
    case e: NumberFormatException => None
  }

  def getFloat(p: Option[String]): Option[Float] = p.flatMap(getFloat)

  def getFloat(p: String): Option[Float] = try {
    if (Strings.blankp(p))
      None
    else
      Some(p.trim.toFloat)
  } catch {
    case e: NumberFormatException => None
  }

  def getDouble(p: Option[String]): Option[Double] = p.flatMap(getDouble)

  def getDouble(p: String): Option[Double] = try {
    if (Strings.blankp(p))
      None
    else
      Some(p.trim.toDouble)
  } catch {
    case e: NumberFormatException => None
  }

  def charToInt(p: Char): Int = {
    val v = p - '0'
    if (v > 9)
      throw new NumberFormatException(p.toString)
    else
      v
  }

  def parse(p: String): ParseResult[Number] = try {
    ParseResult.success(AnyUtils.toNumber(p))
  } catch {
    case NonFatal(e) => ParseResult.error(s"Invalid number: $p")
  }

  def parse(labelf: String => Option[Number], p: String): ParseResult[Number] = {
    def msg = s"Invalid label or number: $p"
    for {
      a <- ParseResult.executeDebug[Option[Number]](msg)(labelf(p))
      b <- a.map(ParseResult.success(_)) getOrElse {
        try {
          ParseResult.success(AnyUtils.toNumber(p))
        } catch {
          case NonFatal(e) => ParseResult.error(msg)
        }
      }
    } yield b
  }
}

