package org.goldenport.cli

import org.goldenport.parser.CommandParser

/*
 * @since   Oct.  6, 2018
 *  version Feb. 24, 2019
 * @version Oct. 14, 2019
 * @author  ASAMI, Tomoharu
 */
sealed trait Response {
  def text: String
}

case class ErrorResponse(
  code: Int,
  message: String,
  detail: Option[Int] = None
) extends Response {
  def text = message
}

case object VoidResponse extends Response {
  val text = ""
}

case class StringResponse(
  string: String
) extends Response {
  def text = string
}

object Response {
  val empty = VoidResponse

  def apply(): Response = VoidResponse

  def apply(p: String): Response = StringResponse(p)

  def notFound(
    cmd: String,
    candidates: Option[CommandParser.Candidates[Engine.Candidate]]
  ): Response = {
    val msg = candidates.
      map(cs => s"""Command '$cmd' is ambiguous: (${cs.commands.vector.map(_.name).mkString(", ")})""").
      getOrElse(s"Command '$cmd' not found. ")
    ErrorResponse(404, msg)
  }
}
