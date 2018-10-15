package org.goldenport.parser

import scalaz._, Scalaz._  
import org.goldenport.i18n.I18NString
  
/*
 * @since   Aug. 21, 2018
 * @version Aug. 26, 2018
 * @author  ASAMI, Tomoharu
 */
trait ParseMessage {
}

case class ErrorMessage(msg: I18NString, location: Option[ParseLocation]) extends ParseMessage {
}

case class WarningMessage(msg: I18NString, location: Option[ParseLocation]) extends ParseMessage {
}
