package org.goldenport.util

/*
 * @since   Mar. 12, 2020
 * @version Mar. 12, 2020
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
}

