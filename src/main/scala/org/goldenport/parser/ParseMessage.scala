package org.goldenport.parser

import scalaz._, Scalaz._  
import java.net.{URI, URL}
import java.io.File
import org.goldenport.i18n.I18NString
import org.goldenport.cli.Environment
  
/*
 * @since   Aug. 21, 2018
 *  version Oct. 14, 2018
 *  version Feb.  2, 2019
 *  version Sep.  6, 2020
 * @version Jan. 16, 2021
 * @author  ASAMI, Tomoharu
 */
trait ParseMessage {
  def msg: I18NString
  def location: Option[ParseLocation]

  def withLocation(uri: URI): ParseMessage
  def withLocation(url: URL): ParseMessage
  def withLocation(file: File): ParseMessage
  def complementLocation(uri: URI): ParseMessage
  def complementLocation(url: URL): ParseMessage
  def complementLocation(file: File): ParseMessage

  def en: String = location.map(x => s"${x.show}: ${msg.en}").getOrElse(msg.en)
  def en(workdir: File): String = location.map(x => s"${x.show(workdir)}: ${msg.en}").getOrElse(msg.en)
  def en(env: Environment): String = location.map(x => s"${x.show(env)}: ${msg.en}").getOrElse(msg.en)
}

case class ErrorMessage(
  msg: I18NString,
  location: Option[ParseLocation]
) extends ParseMessage {
  def withLocation(uri: URI): ErrorMessage = copy(location = location.map(_.withLocation(uri)))
  def withLocation(url: URL): ErrorMessage = withLocation(url.toURI)
  def withLocation(file: File): ErrorMessage = withLocation(file.toURI)
  def complementLocation(uri: URI): ErrorMessage = copy(location = location.map(_.complementLocation(uri)))
  def complementLocation(url: URL): ErrorMessage = complementLocation(url.toURI)
  def complementLocation(file: File): ErrorMessage = complementLocation(file.toURI)
}
object ErrorMessage {
  def apply(location: ParseLocation, msg: String): ErrorMessage = ErrorMessage(
    I18NString(msg),
    Some(location)
  )

  def apply(location: Option[ParseLocation], msg: String): ErrorMessage = ErrorMessage(
    I18NString(msg),
    location
  )

  def apply(msg: String): ErrorMessage = ErrorMessage(
    I18NString(msg),
    None
  )

  def apply(en: String, ja: String): ErrorMessage = ErrorMessage(
    I18NString(en, ja),
    None
  )

  def apply(en: String, ja: String, location: Option[ParseLocation]): ErrorMessage = ErrorMessage(
    I18NString(en, ja),
    location
  )

  def apply(e: Throwable): ErrorMessage = ErrorMessage(
    I18NString(e.getMessage),
    None
  )
}

case class WarningMessage(
  msg: I18NString,
  location: Option[ParseLocation]
) extends ParseMessage {
  def withLocation(uri: URI): WarningMessage = copy(location = location.map(_.withLocation(uri)))
  def withLocation(url: URL): WarningMessage = withLocation(url.toURI)
  def withLocation(file: File): WarningMessage = withLocation(file.toURI)
  def complementLocation(uri: URI): WarningMessage = copy(location = location.map(_.complementLocation(uri)))
  def complementLocation(url: URL): WarningMessage = complementLocation(url.toURI)
  def complementLocation(file: File): WarningMessage = complementLocation(file.toURI)
}
object WarningMessage {
  def apply(location: ParseLocation, msg: String): WarningMessage = WarningMessage(
    I18NString(msg),
    Some(location)
  )

  def apply(location: Option[ParseLocation], msg: String): WarningMessage = WarningMessage(
    I18NString(msg),
    location
  )

  def apply(msg: String): WarningMessage = WarningMessage(
    I18NString(msg),
    None
  )

  def apply(e: Throwable): WarningMessage = WarningMessage(
    I18NString(e.getMessage),
    None
  )
}
