package org.goldenport.parser

import scalaz._, Scalaz._  
import org.goldenport.i18n.I18NString
  
/*
 * @since   Aug. 21, 2018
 *  version Oct. 14, 2018
 *  version Feb.  2, 2019
 * @version Sep.  6, 2020
 * @author  ASAMI, Tomoharu
 */
trait ParseMessage {
  def msg: I18NString
  def location: Option[ParseLocation]

  def en: String = location.map(x => s"${x.show}: $msg").getOrElse(msg.en)
}

case class ErrorMessage(
  msg: I18NString,
  location: Option[ParseLocation]
) extends ParseMessage {
}
object ErrorMessage {
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
}
object WarningMessage {
  def apply(msg: String): WarningMessage = WarningMessage(
    I18NString(msg),
    None
  )

  def apply(e: Throwable): WarningMessage = WarningMessage(
    I18NString(e.getMessage),
    None
  )
}
