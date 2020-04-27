package org.goldenport.cli

import java.io.File
import org.goldenport.parser.CommandParser
import org.goldenport.bag.Bag
import org.goldenport.realm.Realm
import org.goldenport.io.IoUtils

/*
 * @since   Oct.  6, 2018
 *  version Feb. 24, 2019
 *  version Oct. 14, 2019
 *  version Feb. 18, 2020
 * @version Mar.  1, 2020
 * @author  ASAMI, Tomoharu
 */
sealed trait Response {
  def text: String = stdout orElse stderr getOrElse ""
  def stdout: Option[String] = None
  def stderr: Option[String] = None
  def output(env: Environment): Unit = {}
}

case class ErrorResponse(
  code: Int,
  message: String,
  detail: Option[Int] = None
) extends Response {
  override def stdout = Some(message)
}

case object VoidResponse extends Response {
}

case class StringResponse(
  string: String
) extends Response {
  override def stdout = Some(string)
}

case class FileResponse(
  bag: Bag
) extends Response {
  override def stdout = Some("[BAG]")
  override def output(env: Environment): Unit = {
    val file = new File(env.outputDirectory, bag.name)
    IoUtils.save(file, bag)
  }
}

case class FileRealmResponse(
  realm: Realm
) extends Response {
  override def stdout = Some("[REALM]")
  override def output(env: Environment): Unit = {
    implicit val ctx = Realm.Context.create(env.config)
    realm.export(ctx)
  }
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
