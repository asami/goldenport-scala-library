package org.goldenport.context

import scalaz._, Scalaz._
import java.util.Locale
import org.goldenport.i18n.{I18NString, I18NTemplate, I18NMessage}
import org.goldenport.extension.IRecord
import org.goldenport.collection.NonEmptyVector
import org.goldenport.context.DetailCode.Reaction
import org.goldenport.cli.spec
import org.goldenport.util.StringUtils
import org.goldenport.util.AnyUtils
import Fault._

/*
 * @since   Feb. 21, 2021
 *  version Feb. 22, 2021
 *  version Mar. 27, 2021
 *  version Apr. 29, 2021
 *  version May. 27, 2021
 *  version Jun. 20, 2021
 *  version Oct. 25, 2021
 *  version Nov. 15, 2021
 *  version Jan. 28, 2022
 *  version Feb.  1, 2022
 *  version Mar.  6, 2022
 *  version Jun. 13, 2022
 *  version Sep.  1, 2022
 *  version Oct. 26, 2022
 *  version Sep. 28, 2023
 *  version Nov. 11, 2023
 * @version Mar. 16, 2025
 * @author  ASAMI, Tomoharu
 */
sealed trait Fault extends Incident {
  def name = StringUtils.classNameToHypenName("Fault", this)
//  def messageTemplate: I18NTemplate
  def message: I18NMessage
  def reaction: Reaction
  def properties(locale: Locale): IRecord
  def implicitStatusCode: StatusCode

  def RAISE: Nothing = throw new FaultException(this)

  def toPayload: Fault.Payload = Fault.Payload(
    name,
    message.toPayload,
    reaction.toPayload,
    implicitStatusCode.toPayload
  )
}

sealed trait Parameters1 extends { self: Fault =>
  def parameters: Option[Seq[Any]]
//  def message = messageTemplate.toI18NMessage(parameters.map(AnyUtils.toString).mkString(";"))

  def properties(locale: Locale): IRecord = IRecord.dataS(
    KEY_NAME -> name,
    KEY_MESSAGE -> message(locale)
  ) + IRecord.dataOptionS(
    KEY_PARAMETERS -> parameters
  )
}

sealed trait MessageTemplateImpl { slef: Fault =>
  def parameters: Seq[Any]
  def messageTemplate: I18NTemplate

  def message = messageTemplate.toI18NMessage(parameters.mkString(";"))

  def properties(locale: Locale): IRecord = IRecord.dataS(
    KEY_NAME -> name,
    KEY_PARAMETERS -> parameters,
    KEY_MESSAGE -> message(),
    KEY_LOCAL_MESSAGE -> message(locale)
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
  val KEY_VALUES = 'values
  val KEY_MESSAGE = 'message
  val KEY_LOCAL_MESSAGE = 'local_message

  case class Payload(
    name: String,
    message: I18NMessage.Payload,
    reaction: DetailCode.Reaction.Payload,
    implicitStatusCode: StatusCode.Payload
  ) {
    def restore: Fault = RemoteFault(
      name,
      message.restore,
      reaction.restore,
      implicitStatusCode.restore
    )
  }

  case class RemoteFault(
    override val name: String,
    message: I18NMessage,
    reaction: DetailCode.Reaction,
    implicitStatusCode: StatusCode
  ) extends Fault {
    def properties(locale: Locale): IRecord = IRecord.dataS(
      KEY_NAME -> name,
      KEY_MESSAGE -> message(locale)
    )
  }
}

trait ValueDomainFault extends Fault {
  def reaction = Reaction.ClientInput
}
object ValueDomainFault {
  def apply(p: I18NString): ValueDomainFault = ValueDomainValueFault(p)
  def apply(p: String): ValueDomainFault = ValueDomainValueFault(p)
  def apply(labal: String, value: String): ValueDomainValueFault = ValueDomainValueFault(value, I18NTemplate(labal))
  def apply(name: String, datatype: spec.DataType, value: Any) =
    ValueDomainDatatypeFault(name, datatype, AnyUtils.toString(value))
}

case class ValueDomainValueFault(
  value: String = "",
  messageTemplate: I18NTemplate = ValueDomainValueFault.template
) extends ValueDomainFault {
  def implicitStatusCode: StatusCode = StatusCode.BadRequest

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
  def implicitStatusCode: StatusCode = StatusCode.BadRequest

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

case class ValueDomainDatatypeFault(
  key: String,
  datatype: spec.DataType,
  value: String = "",
  messageTemplate: I18NTemplate = ValueDomainDatatypeFault.template
) extends ValueDomainFault {
  def implicitStatusCode: StatusCode = StatusCode.BadRequest

  def message = messageTemplate.toI18NMessage(key, datatype, value)

  def properties(locale: Locale): IRecord = IRecord.dataS(
    KEY_NAME -> name,
    KEY_VALUE -> value,
    KEY_MESSAGE -> message(locale)
  )
}
object ValueDomainDatatypeFault {
  val template = I18NTemplate("Invalid value {1}[{2}]: {3}")

  def apply(key: String, datatype: spec.DataType, value: Any): ValueDomainDatatypeFault =
    new ValueDomainDatatypeFault(key, datatype, AnyUtils.toEmbed(value))

  // def apply(p: I18NString): ValueDomainDatatypeFault =
  //   ValueDomainDatatypeFault(messageTemplate = I18NTemplate(p))
}

sealed trait ArgumentFault extends Fault {
  def implicitStatusCode: StatusCode = StatusCode.BadRequest
  def reaction = Reaction.ClientInput
}

case class InvalidArgumentFault(
  message: I18NMessage,
  parameter: Option[String] = None,
  value: Option[String] = None,
  faults: Option[NonEmptyVector[Fault]] = None
) extends ArgumentFault {
  def properties(locale: Locale): IRecord = IRecord.dataS(
    KEY_NAME -> name,
    KEY_MESSAGE -> message(locale)
  ) + IRecord.dataOptionS(
    KEY_PARAMETER -> parameter,
    KEY_VALUE -> value
  )
} 
object InvalidArgumentFault {
  val template = I18NTemplate("Invalid Argument[{0}]: {1}")

  def apply(p: I18NString): InvalidArgumentFault = InvalidArgumentFault(p.toI18NMessage)
  def apply(p: String, fault: Fault): InvalidArgumentFault = {
    val msg = _message(p, List(fault))
    InvalidArgumentFault(msg, Some(p), None, Some(NonEmptyVector(fault)))
  }
  def apply(p: String, faults: Iterable[Fault]): InvalidArgumentFault = {
    val msg = _message(p, faults)
    InvalidArgumentFault(msg, Some(p), None, NonEmptyVector.createOption(faults))
  }
  def apply(p: String, faults: NonEmptyList[Fault]): InvalidArgumentFault = {
    val msg = _message(p, faults.toList)
    InvalidArgumentFault(msg, Some(p), None, Some(NonEmptyVector(faults)))
  }

  private def _message(key: String, ps: Iterable[Fault]): I18NMessage = {
    ps.toVector.map(_.message).concatenate
  }
  // .map(_.message).list.mkString(";"))))
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

  def apply(p: String): MissingArgumentFault = MissingArgumentFault(List(p))
}

case class TooManyArgumentsFault(
  values: Seq[Any] = Nil,
  messageTemplate: I18NTemplate = TooManyArgumentsFault.template
) extends ArgumentFault {
  def message = messageTemplate.toI18NMessage(values.map(AnyUtils.toShow).mkString(";"))

  def properties(locale: Locale): IRecord = IRecord.dataS(
    KEY_NAME -> name,
    KEY_VALUES -> values,
    KEY_MESSAGE -> message(),
    KEY_LOCAL_MESSAGE -> message(locale)
  )
}
object TooManyArgumentsFault {
  val template = I18NTemplate("Too many arguments: {0}")
}

sealed trait ResultFault extends Fault {
  def reaction = Reaction.SystemDefect
}

case class ValueDomainResultFault(
  parameters: Seq[String] = Nil,
  messageTemplate: I18NTemplate = ValueDomainResultFault.template
) extends ResultFault {
  def implicitStatusCode: StatusCode = StatusCode.InternalServerError

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

  def apply(p: String): ValueDomainResultFault = apply(I18NString(p))

  def apply(p: I18NString): ValueDomainResultFault =
    ValueDomainResultFault(messageTemplate = I18NTemplate(p))

  def apply(p: I18NMessage): ValueDomainResultFault =
    ValueDomainResultFault(messageTemplate = I18NTemplate(p))
}

sealed trait PropertyFault extends Fault {
  def reaction = Reaction.SystemDefect
}

case class InvalidPropertyFault(
  parameters: Seq[String] = Nil,
  messageTemplate: I18NTemplate = InvalidPropertyFault.template
) extends PropertyFault {
  def implicitStatusCode: StatusCode = StatusCode.BadRequest

  def message = messageTemplate.toI18NMessage(parameters.mkString(";"))

  def properties(locale: Locale): IRecord = IRecord.dataS(
    KEY_NAME -> name,
    KEY_PARAMETERS -> parameters,
    KEY_MESSAGE -> message(),
    KEY_LOCAL_MESSAGE -> message(locale)
  )
}
object InvalidPropertyFault {
  val template = I18NTemplate("Invalid property: {0}")

  def apply(p: String): InvalidPropertyFault = apply(I18NString(p))

  def apply(p: I18NString): InvalidPropertyFault =
    InvalidPropertyFault(messageTemplate = I18NTemplate(p))

  def apply(p: I18NMessage): InvalidPropertyFault =
    InvalidPropertyFault(messageTemplate = I18NTemplate(p))
}

case class MissingPropertyFault(
  parameters: Seq[String] = Nil,
  messageTemplate: I18NTemplate = MissingPropertyFault.template
) extends PropertyFault {
  def implicitStatusCode: StatusCode = StatusCode.BadRequest

  def message = messageTemplate.toI18NMessage(parameters.mkString(";"))

  def properties(locale: Locale): IRecord = IRecord.dataS(
    KEY_NAME -> name,
    KEY_PARAMETERS -> parameters,
    KEY_MESSAGE -> message(),
    KEY_LOCAL_MESSAGE -> message(locale)
  )
}
object MissingPropertyFault {
  val template = I18NTemplate("Missing property: {0}")

  def apply(p: String): MissingPropertyFault = apply(I18NString(p))

  def apply(p: I18NString): MissingPropertyFault =
    MissingPropertyFault(messageTemplate = I18NTemplate(p))

  def apply(p: I18NMessage): MissingPropertyFault =
    MissingPropertyFault(messageTemplate = I18NTemplate(p))
}

case class InvalidTokenFault(
  message: I18NMessage,
  parameters: Option[Seq[Any]] = None
) extends ArgumentFault with Parameters1 {
}
object InvalidTokenFault {
  val template = I18NTemplate("Illegal configuration defect: {0}")

  def apply(label: String, value: String): InvalidTokenFault = apply(value) // TODO

  def apply(p: String): InvalidTokenFault = apply(I18NString(p))

  def apply(p: I18NString): InvalidTokenFault = apply(p.toI18NMessage)

  def apply(ps: Seq[Message]): InvalidTokenFault = apply(ps.toVector.map(_.toI18NMessage).concatenate)

  def parameter(p: Any, ps: Any*): InvalidTokenFault = {
    val xs = p +: ps
    InvalidTokenFault(template.toI18NMessage(xs), parameters = Some(xs))
  }
}

case class ValueDomainPropertyFault(
  parameters: Seq[String] = Nil,
  messageTemplate: I18NTemplate = ValueDomainPropertyFault.template
) extends PropertyFault {
  def implicitStatusCode: StatusCode = StatusCode.BadRequest

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

  def apply(p: String): ValueDomainPropertyFault = apply(I18NString(p))

  def apply(p: I18NString): ValueDomainPropertyFault =
    ValueDomainPropertyFault(messageTemplate = I18NTemplate(p))

  def apply(p: I18NMessage): ValueDomainPropertyFault =
    ValueDomainPropertyFault(messageTemplate = I18NTemplate(p))
}

case class SyntaxErrorFault(
  parameters: Seq[Any] = Nil,
  messageTemplate: I18NTemplate = SyntaxErrorFault.template
) extends ArgumentFault with MessageTemplateImpl {
}
object SyntaxErrorFault {
  val template = I18NTemplate("Syntax error: {0}")

  def apply(p: String): SyntaxErrorFault = apply(I18NString(p))

  def apply(p: I18NString): SyntaxErrorFault =
    SyntaxErrorFault(messageTemplate = I18NTemplate(p))

  def apply(ps: Seq[Message]): SyntaxErrorFault =
    SyntaxErrorFault(messageTemplate = ps.toVector.map(_.toI18NMessage).concatenate.toI18NTemplate)

  def parameter(p: Any, ps: Any*): SyntaxErrorFault = SyntaxErrorFault(p +: ps.toList)
}

case class FormatErrorFault(
  parameters: Seq[Any] = Nil,
  messageTemplate: I18NTemplate = FormatErrorFault.template
) extends ArgumentFault with MessageTemplateImpl {
}
object FormatErrorFault {
  val template = I18NTemplate("Format error: {0}")

  def apply(p: String): FormatErrorFault = apply(I18NString(p))

  def apply(p: I18NString): FormatErrorFault =
    FormatErrorFault(messageTemplate = I18NTemplate(p))

  def apply(ps: Seq[Message]): FormatErrorFault =
    FormatErrorFault(messageTemplate = ps.toVector.map(_.toI18NMessage).concatenate.toI18NTemplate)

  def parameter(p: Any, ps: Any*): FormatErrorFault = FormatErrorFault(p +: ps.toList)
}

case class UnfoldResourceFault(
  parameters: Seq[Any] = Nil,
  messageTemplate: I18NTemplate = UnfoldResourceFault.template
) extends ArgumentFault with MessageTemplateImpl {
}
object UnfoldResourceFault {
  val template = I18NTemplate("Unfold resource: {0}")

  def apply(p: String): UnfoldResourceFault = apply(I18NString(p))

  def apply(p: I18NString): UnfoldResourceFault =
    UnfoldResourceFault(messageTemplate = I18NTemplate(p))

  def apply(ps: Seq[Message]): UnfoldResourceFault =
    UnfoldResourceFault(messageTemplate = ps.toVector.map(_.toI18NMessage).concatenate.toI18NTemplate)

  def parameter(p: Any, ps: Any*): UnfoldResourceFault = UnfoldResourceFault(p +: ps.toList)
}

case class UnsupportedOperationFault(
  parameters: Seq[Any] = Nil,
  messageTemplate: I18NTemplate = UnsupportedOperationFault.template
) extends ArgumentFault with MessageTemplateImpl {
}
object UnsupportedOperationFault {
  val template = I18NTemplate("Unsupported operation: {0}")

  def apply(p: String): UnsupportedOperationFault = apply(I18NString(p))

  def apply(p: I18NString): UnsupportedOperationFault =
    UnsupportedOperationFault(messageTemplate = I18NTemplate(p))

  def apply(ps: Seq[Message]): UnsupportedOperationFault =
    UnsupportedOperationFault(messageTemplate = ps.toVector.map(_.toI18NMessage).concatenate.toI18NTemplate)

  def parameter(p: Any, ps: Any*): UnsupportedOperationFault = UnsupportedOperationFault(p +: ps.toList)
}

case class UnsupportedFormatFault(
  parameters: Seq[Any] = Nil,
  messageTemplate: I18NTemplate = UnsupportedFormatFault.template
) extends ArgumentFault with MessageTemplateImpl {
}
object UnsupportedFormatFault {
  val template = I18NTemplate("Unsupported format: {0}")

  def apply(p: String): UnsupportedFormatFault = apply(I18NString(p))

  def apply(p: I18NString): UnsupportedFormatFault =
    UnsupportedFormatFault(messageTemplate = I18NTemplate(p))

  def apply(ps: Seq[Message]): UnsupportedFormatFault =
    UnsupportedFormatFault(messageTemplate = ps.toVector.map(_.toI18NMessage).concatenate.toI18NTemplate)

  def parameter(p: Any, ps: Any*): UnsupportedFormatFault = UnsupportedFormatFault(p +: ps.toList)
}

sealed trait IoFault extends Fault {
  def reaction = Reaction.SystemDefect
}

case class DatabaseIoFault(
  parameters: Seq[Any] = Nil,
  messageTemplate: I18NTemplate = DatabaseIoFault.template
) extends IoFault with MessageTemplateImpl {
  def implicitStatusCode: StatusCode = StatusCode.InternalServerError
}
object DatabaseIoFault {
  val template = I18NTemplate("Database I/O error: {0}")

  def apply(p: String): DatabaseIoFault = apply(I18NString(p))

  def apply(p: I18NString): DatabaseIoFault = 
    DatabaseIoFault(messageTemplate = I18NTemplate(p))

  def parameter(p: Any, ps: Any*): DatabaseIoFault = DatabaseIoFault(p +: ps.toList)
}

case class NetworkIoFault(
  parameters: Seq[Any] = Nil,
  messageTemplate: I18NTemplate = NetworkIoFault.template
) extends IoFault with MessageTemplateImpl {
  def implicitStatusCode: StatusCode = StatusCode.InternalServerError
}
object NetworkIoFault {
  val template = I18NTemplate("Network I/O error: {0}")

  def apply(p: String): NetworkIoFault = apply(I18NString(p))

  def apply(p: I18NString): NetworkIoFault = 
    NetworkIoFault(messageTemplate = I18NTemplate(p))

  def parameter(p: Any, ps: Any*): NetworkIoFault = NetworkIoFault(p +: ps.toList)
}
case class FileIoFault(
  parameters: Seq[Any] = Nil,
  messageTemplate: I18NTemplate = FileIoFault.template
) extends IoFault with MessageTemplateImpl {
  def implicitStatusCode: StatusCode = StatusCode.InternalServerError
}
object FileIoFault {
  val template = I18NTemplate("File I/O error: {0}")

  def apply(p: String): FileIoFault = apply(I18NString(p))

  def apply(p: I18NString): FileIoFault = 
    FileIoFault(messageTemplate = I18NTemplate(p))

  def parameter(p: Any, ps: Any*): FileIoFault = FileIoFault(p +: ps.toList)
}
case class SystemIoFault(
  parameters: Seq[Any] = Nil,
  messageTemplate: I18NTemplate = SystemIoFault.template
) extends IoFault with MessageTemplateImpl {
  def implicitStatusCode: StatusCode = StatusCode.InternalServerError
}
object SystemIoFault {
  val template = I18NTemplate("System I/O error: {0}")

  def apply(p: String): SystemIoFault = apply(I18NString(p))

  def apply(p: I18NString): SystemIoFault = 
    SystemIoFault(messageTemplate = I18NTemplate(p))

  def parameter(p: Any, ps: Any*): SystemIoFault = SystemIoFault(p +: ps.toList)
}
case class SubsystemIoFault(
  parameters: Seq[Any] = Nil,
  messageTemplate: I18NTemplate = SubsystemIoFault.template
) extends IoFault with MessageTemplateImpl {
  def implicitStatusCode: StatusCode = StatusCode.InternalServerError
}
object SubsystemIoFault {
  val template = I18NTemplate("Subsystem I/O error: {0}")

  def apply(p: String): SubsystemIoFault = apply(I18NString(p))

  def apply(p: I18NString): SubsystemIoFault = 
    SubsystemIoFault(messageTemplate = I18NTemplate(p))

  def parameter(p: Any, ps: Any*): SubsystemIoFault = SubsystemIoFault(p +: ps.toList)
}

case class IllegalConfigurationDefect(
  message: I18NMessage,
  parameters: Option[Seq[Any]] = None
//  messageTemplate: I18NTemplate = IllegalConfigurationDefect.template
) extends Defect with Parameters1 {
  def implicitStatusCode: StatusCode = StatusCode.InternalServerError
}
object IllegalConfigurationDefect {
  val template = I18NTemplate("Illegal configuration defect: {0}")

  def apply(p: String): IllegalConfigurationDefect = apply(I18NString(p))

  def apply(p: I18NString): IllegalConfigurationDefect = apply(p.toI18NMessage)

  def parameter(p: Any, ps: Any*): IllegalConfigurationDefect = {
    val xs = p +: ps
    IllegalConfigurationDefect(template.toI18NMessage(xs), parameters = Some(xs))
  }
}

case class UnmarshallingDefect(
  parameters: Seq[String] = Nil,
  messageTemplate: I18NTemplate = UnmarshallingDefect.template
) extends PropertyFault {
  def implicitStatusCode: StatusCode = StatusCode.BadRequest

  def message = messageTemplate.toI18NMessage(parameters.mkString(";"))

  def properties(locale: Locale): IRecord = IRecord.dataS(
    KEY_NAME -> name,
    KEY_PARAMETERS -> parameters,
    KEY_MESSAGE -> message(),
    KEY_LOCAL_MESSAGE -> message(locale)
  )
}
object UnmarshallingDefect {
  val template = I18NTemplate("Unmarshalling error: {0}")

  def apply(p: String): UnmarshallingDefect =
    UnmarshallingDefect(messageTemplate = I18NTemplate(p))

  def apply(p: I18NString): UnmarshallingDefect =
    UnmarshallingDefect(messageTemplate = I18NTemplate(p))

  def apply(p: I18NMessage): UnmarshallingDefect =
    UnmarshallingDefect(messageTemplate = I18NTemplate(p))
}

case class NoReachDefect(
  parameters: Seq[String] = Nil,
  messageTemplate: I18NTemplate = NoReachDefect.template
) extends PropertyFault {
  def implicitStatusCode: StatusCode = StatusCode.BadRequest

  def message = messageTemplate.toI18NMessage(parameters.mkString(";"))

  def properties(locale: Locale): IRecord = IRecord.dataS(
    KEY_NAME -> name,
    KEY_PARAMETERS -> parameters,
    KEY_MESSAGE -> message(),
    KEY_LOCAL_MESSAGE -> message(locale)
  )
}
object NoReachDefect {
  val template = I18NTemplate("NoReach error: {0}")

  def apply(p: String): NoReachDefect =
    NoReachDefect(messageTemplate = I18NTemplate(p))

  def apply(p: I18NString): NoReachDefect =
    NoReachDefect(messageTemplate = I18NTemplate(p))

  def apply(p: I18NMessage): NoReachDefect =
    NoReachDefect(messageTemplate = I18NTemplate(p))
}

case class Faults(faults: Vector[Fault] = Vector.empty) {
  def getMessage: Option[String] = toI18NStringONev.map(x =>
    x.list.map(_.en).mkString(";")
  )

  def getMessage(locale: Locale): Option[String] = toI18NStringONev.map(x =>
    x.list.map(_.apply(locale)).mkString(";")
  )

  def argumentFaults: Vector[ArgumentFault] = faults.collect {
    case m: ArgumentFault => m
  }

  def toI18NStringONev: Option[NonEmptyVector[I18NString]] =
    faults.headOption.map(x => NonEmptyVector(x.message.toI18NString, faults.tail.map(_.message.toI18NString)))

  def getStringI18N: Option[I18NString] = toI18NStringONev.map(x =>
    I18NString.mkI18NString(x.list, ";")
  )

  def getMessageI18N: Option[I18NMessage] = faults.headOption.map(i =>
    faults.tail.foldLeft(i.message)((z, x) => z concat x.message)
  )

  def guessStatusCode: StatusCode = faults.
    map(_.implicitStatusCode).sortWith(_is_strong).
    headOption.getOrElse(StatusCode.InternalServerError)

  def +(p: Faults) = copy(faults = faults ++ p.faults)

  def add(p: Fault) = copy(faults = faults :+ p)

  def :+(p: Fault) = add(p)

  private def _is_strong(lhs: StatusCode, rhs: StatusCode): Boolean = lhs.code >= rhs.code

  def toPayload = Faults.Payload(faults.map(_.toPayload))

  def RAISE_IF_FAILURE: Unit =
    if (faults.nonEmpty)
      InvalidArgumentFault("Invalid Arguments", faults).RAISE
}
object Faults {
  @SerialVersionUID(1L)
  case class Payload(
    faults: Vector[Fault.Payload]
  ) {
    def restore: Faults = Faults(faults.map(_.restore))
  }

  val empty = Faults()

  def apply(p: Fault, ps: Fault*): Faults = Faults((p +: ps).toVector)

  def apply(ps: Iterable[Fault]): Faults = Faults(ps.toVector)
}
