package org.goldenport.context

import java.util.Locale
import org.goldenport.i18n.{I18NString, I18NTemplate, I18NMessage}
import org.goldenport.extension.IRecord
import org.goldenport.context.DetailCode.Reaction
import org.goldenport.util.StringUtils
import Fault._

/*
 * @since   Feb. 21, 2021
 *  version Feb. 22, 2021
 *  version Mar. 27, 2021
 * @version Apr. 10, 2021
 * @author  ASAMI, Tomoharu
 */
sealed trait Fault extends Incident {
  def name = StringUtils.classNameToHypenName("Fault", this)
  def messageTemplate: I18NTemplate
  def message: I18NMessage
  def reaction: Reaction
  def properties(locale: Locale): IRecord
}

object Fault {
  val KEY_NAME = 'name
  val KEY_PARAMETER = 'parameter
  val KEY_PARAMETERS = 'parameters
  val KEY_VALUE = 'value
  val KEY_MESSAGE = 'message
  val KEY_LOCAL_MESSAGE = 'local_message
}

sealed trait ArgumentFault extends Fault {
  def reaction = Reaction.ClientInput
}

case class InvalidArgumentFault(
  parameter: String = "",
  value: String = "",
  messageTemplate: I18NTemplate = InvalidArgumentFault.template
) extends ArgumentFault {
  def message = messageTemplate.toI18NMessage(parameter, value)

  def properties(locale: Locale): IRecord = IRecord.dataS(
    KEY_NAME -> name,
    KEY_PARAMETER -> parameter,
    KEY_VALUE -> value,
    KEY_MESSAGE -> message(locale)
  )
}
object InvalidArgumentFault {
  val template = I18NTemplate("Invalid Argument[{0}]: {1}")

  def apply(p: I18NString): InvalidArgumentFault =
    InvalidArgumentFault(messageTemplate = I18NTemplate(p))
}

case class MissingArgumentFault(
  parameters: Seq[String] = Nil,
  messageTemplate: I18NTemplate = MissingArgumentFault.template
) extends ArgumentFault {
  def message = messageTemplate.toI18NMessage(parameters.mkString(","))

  def properties(locale: Locale): IRecord = IRecord.dataS(
    KEY_NAME -> name,
    KEY_PARAMETERS -> parameters,
    KEY_MESSAGE -> message(locale)
  )
}
object MissingArgumentFault {
  val template = I18NTemplate("Mising Argument: {0}")

}

case class TooMuchArgumentFault(
  parameters: Seq[String] = Nil,
  messageTemplate: I18NTemplate = TooMuchArgumentFault.template
) extends ArgumentFault {
  def message = messageTemplate.toI18NMessage(parameters.mkString(","))

  def properties(locale: Locale): IRecord = IRecord.dataS(
    KEY_NAME -> name,
    KEY_PARAMETERS -> parameters,
    KEY_MESSAGE -> message(),
    KEY_LOCAL_MESSAGE -> message(locale)
  )
}
object TooMuchArgumentFault {
  val template = I18NTemplate("Too much arguments: {0}")
}

sealed trait IoFault extends Fault {
}

case class Faults(faults: Vector[Fault] = Vector.empty) {
  def argumentFaults: Vector[ArgumentFault] = faults.collect {
    case m: ArgumentFault => m
  }
}
object Faults {
  val empty = Faults()

  def apply(ps: Iterable[Fault]): Faults = Faults(ps.toVector)
}
