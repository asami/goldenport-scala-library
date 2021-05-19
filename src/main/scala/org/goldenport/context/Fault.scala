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
 * @version Apr. 29, 2021
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

trait ValueDomainFault extends Fault {
  def reaction = Reaction.ClientInput
}
object ValueDomainFault {
  def apply(p: I18NString): ValueDomainFault = ValueDomainValueFault(p)
}

case class ValueDomainValueFault(
  value: String = "",
  messageTemplate: I18NTemplate = ValueDomainValueFault.template
) extends ValueDomainFault {
  def message = messageTemplate.toI18NMessage(value)

  def properties(locale: Locale): IRecord = IRecord.dataS(
    KEY_NAME -> name,
    KEY_VALUE -> value,
    KEY_MESSAGE -> message(locale)
  )
}
object ValueDomainValueFault {
  val template = I18NTemplate("Invalid value: {1}")

  def apply(p: I18NString): ValueDomainValueFault =
    ValueDomainValueFault(messageTemplate = I18NTemplate(p))
}

case class ValueDomainMultiplicityFault(
  value: String = "",
  messageTemplate: I18NTemplate = ValueDomainMultiplicityFault.template
) extends ValueDomainFault {
  def message = messageTemplate.toI18NMessage(value)

  def properties(locale: Locale): IRecord = IRecord.dataS(
    KEY_NAME -> name,
    KEY_VALUE -> value,
    KEY_MESSAGE -> message(locale)
  )
}
object ValueDomainMultiplicityFault {
  val template = I18NTemplate("Illieal multiplicity: {1}")

  def apply(p: I18NString): ValueDomainMultiplicityFault =
    ValueDomainMultiplicityFault(messageTemplate = I18NTemplate(p))
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

sealed trait ResultFault extends Fault {
  def reaction = Reaction.SystemDefect
}

case class ValueDomainResultFault(
  parameters: Seq[String] = Nil,
  messageTemplate: I18NTemplate = ValueDomainResultFault.template
) extends ResultFault {
  def message = messageTemplate.toI18NMessage(parameters.mkString(","))

  def properties(locale: Locale): IRecord = IRecord.dataS(
    KEY_NAME -> name,
    KEY_PARAMETERS -> parameters,
    KEY_MESSAGE -> message(),
    KEY_LOCAL_MESSAGE -> message(locale)
  )
}
object ValueDomainResultFault {
  val template = I18NTemplate("Value domain fault: {0}")

  def apply(p: I18NString): ValueDomainResultFault =
    ValueDomainResultFault(messageTemplate = I18NTemplate(p))

  def apply(p: I18NMessage): ValueDomainResultFault =
    ValueDomainResultFault(messageTemplate = I18NTemplate(p))
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
