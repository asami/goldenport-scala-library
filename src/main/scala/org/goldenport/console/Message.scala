package org.goldenport.console

/*
 * @since   Jan. 10, 2021
 * @version Jan. 10, 2021
 * @author  ASAMI, Tomoharu
 */
sealed trait Message {
}

object Message {
  def apply(p: String): StandardMessage = StandardMessage(p)
}

case class Prompt(prompt: String) extends Message {
}

case class StandardMessage(message: String) extends Message {
}

case class ErrorMessage(message: String) extends Message {
}

case class WarningMessage(message: String) extends Message {
}
