package org.goldenport.parser

import java.net.{URI, URL}
import java.io.File
import org.goldenport.cli.Environment
import org.goldenport.util.UriUtils

/*
 * @since   Aug. 26, 2018
 *  version Sep.  2, 2018
 *  version Jan.  2, 2019
 *  version Feb.  8, 2019
 * @version Jan. 11, 2021
 * @author  ASAMI, Tomoharu
 */
case class ParseLocation(
  uri: Option[URI],
  line: Option[Int],
  offset: Option[Int]
) extends Parser {
  def isEmpty = uri.isEmpty && line.isEmpty && offset.isEmpty
  def toOption = if (isEmpty) None else Some(this)

  def withLocation(p: URI): ParseLocation = copy(uri = Some(p))
  def withLocation(p: URL): ParseLocation = withLocation(p.toURI)
  def withLocation(p: File): ParseLocation = withLocation(p.toURI)

  def complementLocation(p: URI): ParseLocation =
    if (uri.isEmpty)
      copy(uri = Some(p))
    else
      this
  def complementLocation(p: URL): ParseLocation = complementLocation(p.toURI)
  def complementLocation(p: File): ParseLocation = complementLocation(p.toURI)

  def adjustPrefix(prefix: Option[String]): ParseLocation =
    prefix.map(adjustPrefix).getOrElse(this)

  def adjustPrefix(prefix: String): ParseLocation =
    offset.map(x => copy(offset = Some(math.max(x - prefix.length, 1)))).
      getOrElse(this)

  def show: String = s"[${_show_uri}${_show_line_offset}]"
  def show(workdir: File): String = s"[${_show_uri(workdir)}${_show_line_offset}]"
  def show(env: Environment): String = s"[${_show_uri(env)}${_show_line_offset}]"

  private def _show_uri = uri.fold("")(x => s"$x: ")
  private def _show_uri(workdir: File) = uri.fold("")(x => s"${UriUtils.showTerse(workdir, x)}: ")
  private def _show_uri(env: Environment) = uri.fold("")(x => s"${UriUtils.showTerse(env, x)}: ")

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
