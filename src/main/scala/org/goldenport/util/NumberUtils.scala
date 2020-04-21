package org.goldenport.util

/*
 * @since   Mar. 12, 2020
 * @version Apr. 21, 2020
 * @author  ASAMI, Tomoharu
 */
object NumberUtils {
  def getBoolean(p: String): Option[Boolean] = p.trim.toLowerCase match {
    case "1" => Some(true)
    case "0" => Some(false)
    case "true" => Some(true)
    case "false" => Some(false)
    case _ => None
  }

  def getInt(p: String): Option[Int] = try {
    Some(p.trim.toInt)
  } catch {
    case e: NumberFormatException => None
  }

  def getLong(p: String): Option[Long] = try {
    Some(p.trim.toLong)
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
}

