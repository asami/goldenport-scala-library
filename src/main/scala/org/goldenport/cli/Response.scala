package org.goldenport.cli

import org.goldenport.parser.CommandParser
import org.goldenport.bag.Bag
import org.goldenport.realm.Realm

/*
 * @since   Oct.  6, 2018
 *  version Feb. 24, 2019
 *  version Oct. 14, 2019
 * @version Feb. 18, 2020
 * @author  ASAMI, Tomoharu
 */
sealed trait Response {
  def text: String = stdout orElse stderr getOrElse ""
  def stdout: Option[String] = None
  def stderr: Option[String] = None
  def output(env: Environment): Unit
}

case class ErrorResponse(
  code: Int,
  message: String,
  detail: Option[Int] = None
) extends Response {
  override def stdout = Some(message)

  def output(env: Environment): Unit = ???
}

case object VoidResponse extends Response {
  def output(env: Environment): Unit = {}
}

case class StringResponse(
  string: String
) extends Response {
  override def stdout = Some(string)
  def output(env: Environment): Unit = ???
}

case class FileResponse(
  bag: Bag
) extends Response {
  override def stdout = Some("[BAG]")
  def output(env: Environment): Unit = ???
}

case class FileRealmResponse(
  realm: Realm
) extends Response {
  override def stdout = Some("[REALM]")
  def output(env: Environment): Unit = ???
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
