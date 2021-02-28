package org.goldenport.context

import org.goldenport.config.Config
import org.goldenport.recorder.ForwardRecorder

/*
 * @since   Feb. 21, 2021
 * @version Feb. 22, 2021
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
}
object ErrorMessages {
  val empty = ErrorMessages()
}

case class WarningMessages(messages: List[WarningMessages] = Nil) {
}
object WarningMessages {
  val empty = WarningMessages()
}
