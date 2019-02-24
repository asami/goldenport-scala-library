package org.goldenport.parser

import java.net.URI

/*
 * @since   Aug. 26, 2018
 *  version Sep.  2, 2018
 *  version Jan.  2, 2019
 * @version Feb.  8, 2019
 * @author  ASAMI, Tomoharu
 */
case class ParseLocation(
  uri: Option[URI],
  line: Option[Int],
  offset: Option[Int]
) extends Parser {
  def isEmpty = uri.isEmpty && line.isEmpty && offset.isEmpty
  def toOption = if (isEmpty) None else Some(this)

  def adjustPrefix(prefix: Option[String]): ParseLocation =
    prefix.map(adjustPrefix).getOrElse(this)

  def adjustPrefix(prefix: String): ParseLocation =
    offset.map(x => copy(offset = Some(math.max(x - prefix.length, 1)))).
      getOrElse(this)

  def show: String = s"[${_show_uri}${_show_line_offset}]"

  private def _show_uri = uri.fold("")(x => s"$x:")
  private def _show_line_offset = (line, offset) match {
    case (None, None) => ""
    case (Some(l), None) => s"$l"
    case (None, Some(r)) => s"$r"
    case (Some(l), Some(r)) => s"$l, $r"
  }
}

object ParseLocation {
  val empty = ParseLocation(None, None, None)
  val start = ParseLocation(None, Some(1), Some(1))

  def apply(line: Int, offset: Int): ParseLocation = ParseLocation(
    None,
    Some(line),
    Some(offset)
  )

  def create(line: Int): ParseLocation = ParseLocation(
    None,
    Some(line),
    Some(1)
  )
}
