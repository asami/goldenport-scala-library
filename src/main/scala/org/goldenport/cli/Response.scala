package org.goldenport.cli

import java.io.File
import java.net.{URL, URI}
import org.goldenport.context.Conclusion
import org.goldenport.parser.CommandParser
import org.goldenport.bag.Bag
import org.goldenport.realm.Realm
import org.goldenport.io.IoUtils
import org.goldenport.io.UriUtils

/*
 * @since   Oct.  6, 2018
 *  version Feb. 24, 2019
 *  version Oct. 14, 2019
 *  version Feb. 18, 2020
 *  version Mar.  1, 2020
 *  version May. 24, 2020
 *  version Apr.  4, 2021
 *  version Jan. 30, 2022
 *  version Feb.  1, 2022
 * @version Nov. 25, 2023
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
  override def stderr = Some(message)
}

case class ConclusionResponse(conclusion: Conclusion) extends Response {
  override def stdout = Some(conclusion.message)
}

case object VoidResponse extends Response {
}

case class StringResponse(
  string: String
) extends Response {
  override def stdout = Some(string)
}

case class FileResponse(
  bag: Bag,
  override val stdout: Option[String] = None,
  uri: Option[URI] = None
) extends Response {
  override def output(env: Environment): Unit = {
    val file = uri.map(_uri_file(env)).getOrElse(_bag_file(env))
    IoUtils.save(file, bag)
  }

  private def _bag_file(env: Environment) = new File(env.outputDirectory, bag.filename)

  private def _uri_file(env: Environment)(uri: URI) =
    UriUtils.getFile(uri).getOrElse(Conclusion.unsupportedOperationFault(s"file: $uri").RAISE)
}
object FileResponse {
  def apply(bag: Bag, uri: URI): FileResponse = FileResponse(bag, uri = Some(uri))
  def apply(bag: Bag, url: URL): FileResponse = apply(bag, url.toURI)
}

case class FileRealmResponse(
  realm: Realm,
  override val stdout: Option[String] = None
) extends Response {
  override def output(env: Environment): Unit = {
    implicit val ctx = Realm.Context.create(env.config)
    realm.export(ctx)
  }
}

trait ApplicationResponse extends Response

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
      getOrElse(s"Command '$cmd' not found.")
    ErrorResponse(404, msg)
  }

  def notFound(
    cmd: String
  ): Response = {
    val msg = s"Command '$cmd' not found."
    ErrorResponse(404, msg)
  }

  def notFound(
    cmd: String,
    candidates: Seq[String]
  ): Response = {
    val msg = s"""Command '$cmd' not found. Candidates: (${candidates.mkString(", ")})"""
    ErrorResponse(404, msg)
  }


  def ambiguous(
    cmd: String,
    candidates: CommandParser.Candidates[Engine.Candidate]
  ): Response = {
    val msg = s"""Command '$cmd' is ambiguous: (${candidates.commands.vector.map(_.name).mkString(", ")})"""
    ErrorResponse(404, msg)
  }

  def ambiguous(
    cmd: String,
    candidates: Seq[String]
  ): Response = {
    val msg = s"""Command '$cmd' is ambiguous: (${candidates.mkString(", ")})"""
    ErrorResponse(404, msg)
  }
}
