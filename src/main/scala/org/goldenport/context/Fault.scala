package org.goldenport.context

import java.util.Locale
import org.goldenport.i18n.{I18NString, I18NTemplate, I18NMessage}
import org.goldenport.extension.IRecord
import org.goldenport.context.DetailCode.Reaction
import org.goldenport.util.StringUtils
import org.goldenport.util.AnyUtils
import Fault._

/*
 * @since   Feb. 21, 2021
 *  version Feb. 22, 2021
 *  version Mar. 27, 2021
 *  version Apr. 29, 2021
 * @version May. 27, 2021
 * @author  ASAMI, Tomoharu
 */
sealed trait Fault extends Incident {
  def name = StringUtils.classNameToHypenName("Fault", this)
  def messageTemplate: I18NTemplate
  def message: I18NMessage
  def reaction: Reaction
  def properties(locale: Locale): IRecord

  def RAISE: Nothing = throw new FaultException(this)
}

sealed trait Parameters1 extends { self: Fault =>
  def parameters: Seq[Any]
  def message = messageTemplate.toI18NMessage(parameters.map(AnyUtils.toString).mkString(";"))

  def properties(locale: Locale): IRecord = IRecord.dataS(
    KEY_NAME -> name,
    KEY_PARAMETERS -> parameters,
    KEY_MESSAGE -> message(locale)
  )
}

sealed trait Defect extends Fault {
  def reaction = Reaction.SystemDefect
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
  def message = messageTemplate.toI18NMessage(parameters.mkString(";"))

  def properties(locale: Locale): IRecord = IRecord.dataS(
    KEY_NAME -> name,
    KEY_PARAMETERS -> parameters,
    KEY_MESSAGE -> message(locale)
  )
}
object MissingArgumentFault {
  val template = I18NTemplate("Mising Argument: {0}")
}

case class TooManyArgumentFault(
  parameters: Seq[String] = Nil,
  messageTemplate: I18NTemplate = TooManyArgumentFault.template
) extends ArgumentFault {
  def message = messageTemplate.toI18NMessage(parameters.mkString(";"))

  def properties(locale: Locale): IRecord = IRecord.dataS(
    KEY_NAME -> name,
    KEY_PARAMETERS -> parameters,
    KEY_MESSAGE -> message(),
    KEY_LOCAL_MESSAGE -> message(locale)
  )
}
object TooManyArgumentFault {
  val template = I18NTemplate("Too many arguments: {0}")
}

sealed trait ResultFault extends Fault {
  def reaction = Reaction.SystemDefect
}

case class ValueDomainResultFault(
  parameters: Seq[String] = Nil,
  messageTemplate: I18NTemplate = ValueDomainResultFault.template
) extends ResultFault {
  def message = messageTemplate.toI18NMessage(parameters.mkString(";"))

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

sealed trait PropertyFault extends Fault {
  def reaction = Reaction.SystemDefect
}

case class MissingPropertyFault(
  parameters: Seq[String] = Nil,
  messageTemplate: I18NTemplate = MissingPropertyFault.template
) extends PropertyFault {
  def message = messageTemplate.toI18NMessage(parameters.mkString(";"))

  def properties(locale: Locale): IRecord = IRecord.dataS(
    KEY_NAME -> name,
    KEY_PARAMETERS -> parameters,
    KEY_MESSAGE -> message(),
    KEY_LOCAL_MESSAGE -> message(locale)
  )
}
object MissingPropertyFault {
  val template = I18NTemplate("Value domain fault: {0}")

  def apply(p: I18NString): MissingPropertyFault =
    MissingPropertyFault(messageTemplate = I18NTemplate(p))

  def apply(p: I18NMessage): MissingPropertyFault =
    MissingPropertyFault(messageTemplate = I18NTemplate(p))
}

case class ValueDomainPropertyFault(
  parameters: Seq[String] = Nil,
  messageTemplate: I18NTemplate = ValueDomainPropertyFault.template
) extends PropertyFault {
  def message = messageTemplate.toI18NMessage(parameters.mkString(";"))

  def properties(locale: Locale): IRecord = IRecord.dataS(
    KEY_NAME -> name,
    KEY_PARAMETERS -> parameters,
    KEY_MESSAGE -> message(),
    KEY_LOCAL_MESSAGE -> message(locale)
  )
}
object ValueDomainPropertyFault {
  val template = I18NTemplate("Value domain fault: {0}")

  def apply(p: I18NString): ValueDomainPropertyFault =
    ValueDomainPropertyFault(messageTemplate = I18NTemplate(p))

  def apply(p: I18NMessage): ValueDomainPropertyFault =
    ValueDomainPropertyFault(messageTemplate = I18NTemplate(p))
}

sealed trait IoFault extends Fault {
}

case class IllegalConfigurationDefect(
  parameters: Seq[Any],
  messageTemplate: I18NTemplate = IllegalConfigurationDefect.template
) extends Defect with Parameters1 {
}
object IllegalConfigurationDefect {
  val template = I18NTemplate("Illegal configuration defect: {0}")

  def apply(p: String): IllegalConfigurationDefect = parameter(p)

  def apply(p: I18NString): IllegalConfigurationDefect =
    IllegalConfigurationDefect(parameters = Nil, messageTemplate = I18NTemplate(p))

  def apply(p: I18NMessage): IllegalConfigurationDefect =
    IllegalConfigurationDefect(parameters = Nil, messageTemplate = I18NTemplate(p))

  def parameter(p: Any, ps: Any*): IllegalConfigurationDefect =
    IllegalConfigurationDefect(parameters = p +: ps)
}

case class Faults(faults: Vector[Fault] = Vector.empty) {
  def argumentFaults: Vector[ArgumentFault] = faults.collect {
    case m: ArgumentFault => m
  }
}
object Faults {
  val empty = Faults()

  def apply(p: Fault, ps: Fault*): Faults = Faults((p +: ps).toVector)

  def apply(ps: Iterable[Fault]): Faults = Faults(ps.toVector)
}
