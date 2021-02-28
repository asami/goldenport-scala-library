package org.goldenport.util

import scala.util.matching.Regex
import scala.util.matching.Regex.Match
import org.goldenport.parser._

/*
 * @since   Jan. 19, 2021
 * @version Jan. 19, 2021
 * @author  ASAMI, Tomoharu
 */
object RegexUtils {
  def getString(regex: Regex, s: String, i: Int, is: Int*): Option[String] =
    regex.findFirstMatchIn(s).flatMap(getString(_, i +: is))

  def getString(p: Match, i: Int, is: Int*): Option[String] = getString(p, i +: is)

  def getString(p: Match, is: Seq[Int]): Option[String] = {
    val a = is.map(i => Option(p.group(i)).getOrElse("")).mkString
    if (a.isEmpty)
      None
    else
      Some(a)
  }

  def getInt(regex: Regex, s: String, i: Int, is: Int*): Option[Int] =
    regex.findFirstMatchIn(s).flatMap(getInt(_, i +: is))

  def getInt(p: Match, i: Int, is: Int*): Option[Int] = getInt(p, i +: is)

  def getInt(p: Match, is: Seq[Int]): Option[Int] = getString(p, is).flatMap(NumberUtils.getInt)

  def getLong(regex: Regex, s: String, i: Int, is: Int*): Option[Long] =
    regex.findFirstMatchIn(s).flatMap(getLong(_, i +: is))

  def getLong(p: Match, i: Int, is: Int*): Option[Long] = getLong(p, i +: is)

  def getLong(p: Match, is: Seq[Int]): Option[Long] = getString(p, is).flatMap(NumberUtils.getLong)

  def getFloat(regex: Regex, s: String, i: Int, is: Int*): Option[Float] =
    regex.findFirstMatchIn(s).flatMap(getFloat(_, i +: is))

  def getFloat(p: Match, i: Int, is: Int*): Option[Float] = getFloat(p, i +: is)

  def getFloat(p: Match, is: Seq[Int]): Option[Float] = getString(p, is).flatMap(NumberUtils.getFloat)

  def getDouble(regex: Regex, s: String, i: Int, is: Int*): Option[Double] =
    regex.findFirstMatchIn(s).flatMap(getDouble(_, i +: is))

  def getDouble(p: Match, i: Int, is: Int*): Option[Double] = getDouble(p, i +: is)

  def getDouble(p: Match, is: Seq[Int]): Option[Double] = getString(p, is).flatMap(NumberUtils.getDouble)

  def parseDouble(regex: Regex, s: String, i: Int, is: Int*): ParseResult[Double] =
    regex.findFirstMatchIn(s) match {
      case Some(s) => parseDouble(s, i +: is)
      case None => EmptyParseResult()
    }

  def parseDouble(p: Match, i: Int, is: Int*): ParseResult[Double] = parseDouble(p, i +: is)

  def parseDouble(p: Match, is: Seq[Int]): ParseResult[Double] = NumberUtils.parseDouble(getString(p, is))
}
