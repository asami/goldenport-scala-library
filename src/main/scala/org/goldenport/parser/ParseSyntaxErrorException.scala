package org.goldenport.parser

import java.io.File
import java.net.{URL, URI}
import org.goldenport.exception.GoldenportException

/*
 * @since   Feb.  2, 2019
 * @version Jan. 10, 2021
 * @author  ASAMI, Tomoharu
 */
class ParseSyntaxErrorException(
  message: String,
  val errors: Vector[ErrorMessage],
  val warnings: Vector[WarningMessage]
) extends GoldenportException(message) {
  def withUrl(url: URL): ParseSyntaxErrorException = new ParseSyntaxErrorException(
    message,
    errors.map(_.withLocation(url)),
    warnings.map(_.withLocation(url))
  )

  def withUri(uri: URI): ParseSyntaxErrorException = new ParseSyntaxErrorException(
    message,
    errors.map(_.withLocation(uri)),
    warnings.map(_.withLocation(uri))
  )

  def withFile(file: File): ParseSyntaxErrorException = new ParseSyntaxErrorException(
    message,
    errors.map(_.withLocation(file)),
    warnings.map(_.withLocation(file))
  )

  def complementUrl(url: URL): ParseSyntaxErrorException = new ParseSyntaxErrorException(
    message,
    errors.map(_.complementLocation(url)),
    warnings.map(_.complementLocation(url))
  )

  def complementUri(uri: URI): ParseSyntaxErrorException = new ParseSyntaxErrorException(
    message,
    errors.map(_.complementLocation(uri)),
    warnings.map(_.complementLocation(uri))
  )

  def complementFile(file: File): ParseSyntaxErrorException = new ParseSyntaxErrorException(
    message,
    errors.map(_.complementLocation(file)),
    warnings.map(_.complementLocation(file))
  )
}

object ParseSyntaxErrorException {
  def apply(errors: Vector[ErrorMessage], warnings: Vector[WarningMessage]): ParseSyntaxErrorException = {
    val msg = if (errors.nonEmpty)
      errors.map(_.en).mkString("\n")
    else if (warnings.nonEmpty)
      warnings.map(_.en).mkString("\n")
    else
      ""
    new ParseSyntaxErrorException(msg, errors, warnings)
  }
}
