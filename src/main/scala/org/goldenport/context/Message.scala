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
 *  version Oct. 12, 2021
 *  version Jan. 20, 2022
 * @version Jun. 13, 2022
 * @author  ASAMI, Tomoharu
 */
trait Message {
  def toI18NMessage: I18NMessage = toI18NString.toI18NMessage
  def toI18NString: I18NString = toI18NMessage.toI18NString
  def toPayload: Message.Payload = Message.Payload(toI18NMessage.toPayload)
}
object Message {
  case class Payload(message: I18NMessage.Payload)
}

trait ErrorMessage extends Message {
}
object ErrorMessage {
  case class I18NStringErrorMessage(message: I18NString) extends ErrorMessage {
    override def toI18NString = message
  }
}

trait WarningMessage extends Message {
}
object WarningMessage {
  case class I18NStringWarningMessage(message: I18NString) extends WarningMessage {
    override def toI18NString = message
  }
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

  def toPayload = ErrorMessages.Payload(messages.toVector.map(_.toPayload))
}
object ErrorMessages {
  val empty = ErrorMessages()

  @SerialVersionUID(1L)
  case class Payload(
    messages: Vector[Message.Payload]
  ) {
    def restore: ErrorMessages = ErrorMessages(
    )
  }

  def apply(ps: Iterable[ErrorMessage]):  ErrorMessages = new ErrorMessages(ps.toList)
  def apply(ps: Iterator[ErrorMessage]):  ErrorMessages = new ErrorMessages(ps.toList)
}

case class WarningMessages(messages: List[WarningMessage] = Nil) extends Messages {
  def isEmpty = messages.isEmpty
  def toOption: Option[WarningMessages] = if (isEmpty) None else Some(this)

  def +(rhs: WarningMessages) = copy(messages = messages ++ rhs.messages)

  def toPayload = WarningMessages.Payload(messages.toVector.map(_.toPayload))
}
object WarningMessages {
  val empty = WarningMessages()

  @SerialVersionUID(1L)
  case class Payload(
    messages: Vector[Message.Payload]
  ) {
    def restore: WarningMessages = WarningMessages(
    )
  }

  def apply(ps: Iterable[WarningMessage]):  WarningMessages = new WarningMessages(ps.toList)
  def apply(ps: Iterator[WarningMessage]):  WarningMessages = new WarningMessages(ps.toList)
}
