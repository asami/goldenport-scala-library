package org.goldenport.parser

import scalaz._, Scalaz._  
import org.goldenport.i18n.I18NString
  
/*
 * @since   Aug. 21, 2018
 * @version Oct. 14, 2018
 * @author  ASAMI, Tomoharu
 */
trait ParseMessage {
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
}
