package org.goldenport.context

import scalaz._, Scalaz._
import org.goldenport.config.Config
import org.goldenport.i18n.I18NString
import org.goldenport.recorder.ForwardRecorder

/*
 * @since   Feb. 21, 2021
 * @version May. 30, 2021
 * @author  ASAMI, Tomoharu
 */
sealed trait Message {
  def isEmpty: Boolean
  def toOption: Option[Message]
  def message: I18NString
}

sealed trait ErrorMessage extends Message {
}
object ErrorMessage {
}

sealed trait WarningMessage extends Message {
}
object WarningMessage {
}

case class ErrorMessages(messages: List[ErrorMessage] = Nil) {
  def isEmpty = messages.isEmpty
  def toOption: Option[ErrorMessages] = if (isEmpty) None else Some(this)
  def message: I18NString = messages.map(_.message).concatenate

  def +(rhs: ErrorMessages) = copy(messages = messages ++ rhs.messages)

}
object ErrorMessages {
  val empty = ErrorMessages()
}

case class WarningMessages(messages: List[WarningMessage] = Nil) {
  def isEmpty = messages.isEmpty
  def toOption: Option[WarningMessages] = if (isEmpty) None else Some(this)
  def message: I18NString = messages.map(_.message).concatenate

  def +(rhs: WarningMessages) = copy(messages = messages ++ rhs.messages)
}
object WarningMessages {
  val empty = WarningMessages()
}
