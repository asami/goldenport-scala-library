package org.goldenport.util

import scalaz._, Scalaz._
import scala.util.matching.Regex
import scala.util.matching.Regex.Match
import org.goldenport.context.Consequence
import org.goldenport.parser._

/*
 * @since   Jan. 19, 2021
 *  version Mar. 24, 2021
 * @version Sep. 26, 2022
 * @author  ASAMI, Tomoharu
 */
object RegexUtils {
  def getString(regex: Regex, s: String, i: Int): Option[String] =
    regex.findFirstMatchIn(s).flatMap(getString(_, i))

  def getString(p: Match, i: Int): Option[String] = Option(p.group(i))

  def getStrings(p: Match, i: Int, is: Int*): Option[List[Option[String]]] = getStrings(p, i +: is)

  def getStrings(p: Match, is: Seq[Int]): Option[List[Option[String]]] = {
    val a = is.map(i => Option(p.group(i)))
    if (a.isEmpty)
      None
    else
      Some(a.toList)
  }

  def getInt(regex: Regex, s: String, i: Int): Option[Int] =
    regex.findFirstMatchIn(s).flatMap(getInt(_, i))

  def getInt(p: Match, i: Int): Option[Int] = getString(p, i).flatMap(NumberUtils.getInt)

  def getInts(regex: Regex, s: String, i: Int, is: Int*): Option[List[Option[Int]]] =
    regex.findFirstMatchIn(s).flatMap(getInts(_, i +: is))

  def getInts(p: Match, i: Int, is: Int*): Option[List[Option[Int]]] = getInts(p, i +: is)

  def getInts(p: Match, is: Seq[Int]): Option[List[Option[Int]]] = getStrings(p, is).map(_.map(NumberUtils.getInt))

  def getLong(regex: Regex, s: String, i: Int): Option[Long] =
    regex.findFirstMatchIn(s).flatMap(getLong(_, i))

  def getLong(p: Match, i: Int): Option[Long] = getString(p, i).flatMap(NumberUtils.getLong)

  def getLongs(regex: Regex, s: String, i: Int, is: Int*): Option[List[Option[Long]]] =
    regex.findFirstMatchIn(s).flatMap(getLongs(_, i +: is))

  def getLongs(p: Match, i: Int, is: Int*): Option[List[Option[Long]]] = getLongs(p, i +: is)

  def getLongs(p: Match, is: Seq[Int]): Option[List[Option[Long]]] = getStrings(p, is).map(_.map(NumberUtils.getLong))

  def getFloat(regex: Regex, s: String, i: Int): Option[Float] =
    regex.findFirstMatchIn(s).flatMap(getFloat(_, i))

  def getFloat(p: Match, i: Int): Option[Float] = getString(p, i).flatMap(NumberUtils.getFloat)

  def getFloats(regex: Regex, s: String, i: Int, is: Int*): Option[List[Option[Float]]] =
    regex.findFirstMatchIn(s).flatMap(getFloats(_, i +: is))

  def getFloats(p: Match, i: Int, is: Int*): Option[List[Option[Float]]] = getFloats(p, i +: is)

  def getFloats(p: Match, is: Seq[Int]): Option[List[Option[Float]]] = getStrings(p, is).map(_.map(NumberUtils.getFloat))

  def getDouble(regex: Regex, s: String, i: Int): Option[Double] =
    regex.findFirstMatchIn(s).flatMap(getDouble(_, i))

  def getDouble(p: Match, i: Int): Option[Double] = getString(p, i).flatMap(NumberUtils.getDouble)

  def getDoubles(regex: Regex, s: String, i: Int, is: Int*): Option[List[Option[Double]]] =
    regex.findFirstMatchIn(s).flatMap(getDoubles(_, i +: is))

  def getDoubles(p: Match, i: Int, is: Int*): Option[List[Option[Double]]] = getDoubles(p, i +: is)

  def getDoubles(p: Match, is: Seq[Int]): Option[List[Option[Double]]] = getStrings(p, is).map(_.map(NumberUtils.getDouble))

  def parseDouble(regex: Regex, s: String, i: Int): ParseResult[Double] = regex.findFirstMatchIn(s) match {
    case Some(s) => parseDouble(s, i)
    case None => EmptyParseResult()
  }

  def parseDouble(p: Match, i: Int): ParseResult[Double] = NumberUtils.parseDouble(getString(p, i))

  def parseDoubles(regex: Regex, s: String, i: Int, is: Int*): ParseResult[List[Option[Double]]] =
    regex.findFirstMatchIn(s) match {
      case Some(s) => parseDoubles(s, i +: is)
      case None => EmptyParseResult()
    }

  def parseDoubles(p: Match, i: Int, is: Int*): ParseResult[List[Option[Double]]] = parseDoubles(p, i +: is)

  def parseDoubles(p: Match, is: Seq[Int]): ParseResult[List[Option[Double]]] =
    getStrings(p, is) match {
      case Some(s) => s.traverse(_.traverse(NumberUtils.parseDouble))
      case None => ParseResult.error("No match")
    }


  def cAsString(p: Match, i: Int): Consequence[String] =
    Option(p.group(i)) match {
      case Some(s) => Consequence.success(s)
      case None => Consequence.success("")
    }

  def cGetString(p: Match, i: Int): Consequence[Option[String]] =
    Option(p.group(i)) match {
      case Some(s) => Consequence.success(Some(s))
      case None => Consequence.success(None)
    }

  def cAsInt(p: Match, i: Int): Consequence[Int] =
    Option(p.group(i)) match {
      case Some(s) => NumberUtils.consequenceInt(s)
      case None => Consequence.missingPropertyFault(s"Rexex($i)")
    }

  def cGetInt(p: Match, i: Int): Consequence[Option[Int]] =
    Option(p.group(i)) match {
      case Some(s) => NumberUtils.consequenceInt(s).map(Some.apply)
      case None => Consequence.success(None)
    }
}
