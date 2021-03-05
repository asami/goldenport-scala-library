package org.goldenport.exception

import java.io.File
import java.net.{URL, URI}
import org.goldenport.parser.{ParseLocation, ParseFailure}
import org.goldenport.parser.{ErrorMessage, WarningMessage}

/*
 * @snice   Aug. 30, 2017
 *  version Aug. 30, 2017
 *  version Oct. 29, 2017
 *  version Jan. 12, 2018
 *  version Feb.  2, 2019
 *  version Apr. 14, 2019
 *  version Jul. 21, 2019
 * @version Jan. 10, 2021
 * @author  ASAMI, Tomoharu
 */
abstract class GoldenportException(
  message: String,
  cause: Throwable = null
) extends RuntimeException(message, cause) {
}
object GoldenportException {
  def message(m: String, d: String) = if (m != null) m else d
}

class NoReachDefectException(
  m: String = null
) extends GoldenportException(GoldenportException.message(m, "No reach"))

class NotImplementedYetDefectException(
  m: String = null
) extends GoldenportException(GoldenportException.message(m, "Not implemented yet"))

class IllegalConfigurationDefectException(
  m: String = null
) extends GoldenportException(GoldenportException.message(m, "Illegal configuration"))

class MissingPropertyFaultException(
  name: String
) extends GoldenportException(name)

class SyntaxErrorFaultException(
  message: String,
  cause: Throwable = null,
  location: Option[ParseLocation] = None,
  parseFailure: Option[ParseFailure[_]] = None
) extends GoldenportException(message) {
  def withUrl(url: URL): SyntaxErrorFaultException = new SyntaxErrorFaultException(
    message,
    cause,
    location.map(_.withLocation(url)),
    parseFailure.map(_.withLocation(url))
  )

  def withUri(uri: URI): SyntaxErrorFaultException = new SyntaxErrorFaultException(
    message,
    cause,
    location.map(_.withLocation(uri)),
    parseFailure.map(_.withLocation(uri))
  )

  def withFile(file: File): SyntaxErrorFaultException = new SyntaxErrorFaultException(
    message,
    cause,
    location.map(_.withLocation(file)),
    parseFailure.map(_.withLocation(file))
  )

  def complementUrl(url: URL): SyntaxErrorFaultException = new SyntaxErrorFaultException(
    message,
    cause,
    location.map(_.complementLocation(url)),
    parseFailure.map(_.complementLocation(url))
  )

  def complementUri(uri: URI): SyntaxErrorFaultException = new SyntaxErrorFaultException(
    message,
    cause,
    location.map(_.complementLocation(uri)),
    parseFailure.map(_.complementLocation(uri))
  )

  def complementFile(file: File): SyntaxErrorFaultException = new SyntaxErrorFaultException(
    message,
    cause,
    location.map(_.complementLocation(file)),
    parseFailure.map(_.complementLocation(file))
  )

  def errorMessages: Vector[ErrorMessage] = parseFailure.map(_.errors).getOrElse(Vector.empty)

  def warningMessages: Vector[WarningMessage] = parseFailure.map(_.warnings).getOrElse(Vector.empty)
}
object SyntaxErrorFaultException {
  def apply(message: String): SyntaxErrorFaultException =
    new SyntaxErrorFaultException(message, null, None)

  def apply(message: String, location: ParseLocation): SyntaxErrorFaultException =
    new SyntaxErrorFaultException(message, null, Some(location))

  def apply(p: ParseFailure[_]): SyntaxErrorFaultException =
    new SyntaxErrorFaultException(p.message, null, None)
}
