package org.goldenport.context

import scalaz._, Scalaz._
import org.goldenport.collection.NonEmptyVector
import org.goldenport.config.Config
import org.goldenport.i18n.I18NString
import org.goldenport.i18n.I18NMessage
import org.goldenport.recorder.ForwardRecorder

/*
 * @since   Feb. 21, 2021
 *  version May. 30, 2021
 *  version Jun. 20, 2021
 * @version Oct. 12, 2021
 * @author  ASAMI, Tomoharu
 */
trait Message {
  def toI18NMessage: I18NMessage = toI18NString.toI18NMessage
  def toI18NString: I18NString = toI18NMessage.toI18NString
}

trait ErrorMessage extends Message {
}
object ErrorMessage {
}

trait WarningMessage extends Message {
}
object WarningMessage {
}

sealed trait Messages {
  def isEmpty: Boolean
  def toOption: Option[Messages]
  def messages: List[Message]
  def toI18NMessage: I18NMessage = messages.map(_.toI18NMessage).concatenate
  def toI18NStringONev: Option[NonEmptyVector[I18NString]] =
    messages.headOption.map(x => NonEmptyVector(x.toI18NString, messages.tail.map(_.toI18NString)))
}

case class ErrorMessages(messages: List[ErrorMessage] = Nil) extends Messages {
  def isEmpty = messages.isEmpty
  def toOption: Option[ErrorMessages] = if (isEmpty) None else Some(this)

  def +(rhs: ErrorMessages) = copy(messages = messages ++ rhs.messages)
}
object ErrorMessages {
  val empty = ErrorMessages()
}

case class WarningMessages(messages: List[WarningMessage] = Nil) extends Messages {
  def isEmpty = messages.isEmpty
  def toOption: Option[WarningMessages] = if (isEmpty) None else Some(this)

  def +(rhs: WarningMessages) = copy(messages = messages ++ rhs.messages)
}
object WarningMessages {
  val empty = WarningMessages()
}
