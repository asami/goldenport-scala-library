package org.goldenport.parser

import java.net.URI

/*
 * @since   Aug. 26, 2018
 *  version Sep.  2, 2018
 * @version Jan.  2, 2019
 * @author  ASAMI, Tomoharu
 */
case class ParseLocation(
  uri: Option[URI],
  line: Option[Int],
  offset: Option[Int]
) extends Parser {
  def adjustPrefix(prefix: Option[String]): ParseLocation =
    prefix.map(adjustPrefix).getOrElse(this)

  def adjustPrefix(prefix: String): ParseLocation =
    offset.map(x => copy(offset = Some(math.max(x - prefix.length, 1)))).
      getOrElse(this)
}

object ParseLocation {
  val empty = ParseLocation(None, None, None)
  val init = ParseLocation(None, Some(1), Some(1))

  def apply(line: Int, offset: Int): ParseLocation = ParseLocation(
    None,
    Some(line),
    Some(offset)
  )
}
