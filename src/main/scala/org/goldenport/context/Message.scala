package org.goldenport.context

import org.goldenport.config.Config
import org.goldenport.recorder.ForwardRecorder

/*
 * @since   Feb. 21, 2021
 * @version May. 21, 2021
 * @author  ASAMI, Tomoharu
 */
sealed trait Message {
}

sealed trait ErrorMessage extends Message {
}
object ErrorMessage {
}

sealed trait WarningMessage extends Message {

}
object WarningMessage {
}

case class ErrorMessages(messages: List[ErrorMessages] = Nil) {
  def +(rhs: ErrorMessages) = copy(messages = messages ++ rhs.messages)
}
object ErrorMessages {
  val empty = ErrorMessages()
}

case class WarningMessages(messages: List[WarningMessages] = Nil) {
  def +(rhs: WarningMessages) = copy(messages = messages ++ rhs.messages)
}
object WarningMessages {
  val empty = WarningMessages()
}
