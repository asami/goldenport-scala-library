package org.goldenport.context

import scalaz._, Scalaz._
import java.util.Locale
import play.api.libs.json._
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.exception.GoldenportException
import org.goldenport.i18n.I18NString
import org.goldenport.i18n.I18NMessage
import org.goldenport.cli.spec
import org.goldenport.extension.IRecord
import org.goldenport.trace._
import org.goldenport.util.ExceptionUtils
import org.goldenport.util.AnyUtils

/*
 * See org.goldenport.record.v2.Conclusion.
 * 
 * @since   Feb. 21, 2021
 *  version Feb. 25, 2021
 *  version Mar. 26, 2021
 *  version Apr. 29, 2021
 *  version May. 30, 2021
 *  version Jun. 20, 2021
 *  version Oct. 25, 2021
 *  version Nov. 15, 2021
 *  version Dec.  5, 2021
 *  version Jan. 20, 2022
 *  version Feb. 18, 2022
 *  version Apr.  3, 2022
 *  version May. 31, 2022
 *  version Jun. 14, 2022
 *  version Sep. 26, 2022
 *  version Oct. 26, 2022
 *  version Nov. 25, 2022
 *  version Dec. 28, 2022
 *  version Jan. 20, 2023
 *  version Sep. 27, 2023
 *  version Nov.  7, 2023
 *  version Mar. 29, 2025
 * @version May. 11, 2025
 * @author  ASAMI, Tomoharu
 */
case class Conclusion(
  status: StatusCode,
  messageI18NOption: Option[I18NMessage] = None,
  errors: ErrorMessages = ErrorMessages.empty,
  warnings: WarningMessages = WarningMessages.empty,
  exception: Option[Throwable] = None,
  exceptionData: Option[Conclusion.ExceptionData] = None,
  faults: Faults = Faults.empty,
  data: Option[Any] = None, // unused
  trace: Trace = Trace.empty,
  strategy: Conclusion.Strategy = Conclusion.Strategy.none
) {
  import Conclusion._

  def code: Int = status.code
  def messageI18N: I18NMessage = messageI18NOption orElse _errors_message orElse _warnings_message orElse exception.map(x => I18NMessage(x.toString)) orElse exceptionData.flatMap(_.getMessageI18N) orElse faults.getMessageI18N getOrElse status.message

  private def _errors_message: Option[I18NMessage] = errors.toOption.map(_.toI18NMessage)

  private def _warnings_message: Option[I18NMessage] = warnings.toOption.map(_.toI18NMessage)

  def message: String = messageI18N.en
  def message(locale: Locale): String = messageI18N(locale)

  // def incidents: Incidents = trace.incidents
  // def faults: Faults = incidents.faults
  // def effects: Effects = incidents.effects
  // def statictics: Statictics = incidents.statictics

  def withMessage(p: String) = copy(messageI18NOption = Some(I18NMessage(p)))
  def withMessage(p: I18NMessage) = copy(messageI18NOption = Some(p))
  def withTrace(p: TraceHandle): Conclusion = withTrace(p.ctx)
  def withTrace(p: TraceContext): Conclusion = withTrace(p.toTrace)
  def withTrace(p: Trace): Conclusion = copy(trace = p)
  def withException(p: Throwable): Conclusion = copy(exception = Some(p))
  def withExceptionData(p: ExceptionData): Conclusion = copy(exceptionData = Some(p))
  def withData(p: Any): Conclusion = copy(data = Some(p))
  def withDataRecord(p: IRecord): Conclusion = copy(exceptionData = Some(ExceptionData(p)))

  def onErrorPrependMessage(p: String) =
    messageI18NOption match {
      case Some(s) => withMessage(p + message)
      case None =>
        if (faults.nonEmpty)
          copy(faults = faults.prependMessage(p))
        else
          withMessage(p + message)
    }

  def +(rhs: Conclusion): Conclusion = Conclusion(
    status,
    messageI18NOption,
    errors + rhs.errors,
    warnings + rhs.warnings,
    exception, // CAUTION
    exceptionData, // CAUTION
    faults, // CAUTION
    data, // CAUTION
    trace, // CAUTION
    strategy // CAUTION
  )

  def toException: Throwable = exception getOrElse new ConclusionException(this)

  def toConclusionException: Throwable = new ConclusionException(this)

  def RAISE: Nothing = throw toException

  def RAISEC: Nothing = throw toConclusionException

  def isSuccess: Boolean = status.isSuccess

  def isUnauhorized: Boolean = code == StatusCode.Unauthorized.code

  def forConfig: Conclusion = if (isSuccess) this else copy(status = status.forConfig)

  def toPayload: Payload = Payload(
    status.toPayload,
    messageI18NOption.map(_.toPayload),
    errors.toPayload,
    warnings.toPayload,
    exception.map(Payload.ExceptionPayload),
    exceptionData.map(_.toPayload),
    faults.toPayload,
    data,
    trace.toPayload,
    strategy.toPayload
  )
}

object Conclusion {
  val Ok = Conclusion(StatusCode.Ok)
  val BadRequest = Conclusion(StatusCode.BadRequest)
  val Unauthorized = Conclusion(StatusCode.Unauthorized)
  val NotFound = Conclusion(StatusCode.NotFound)
  val InternalServerError = Conclusion(StatusCode.InternalServerError)
  val NotImplemented = Conclusion(StatusCode.NotImplemented)
  //
  val NoReach = Conclusion(StatusCode.NoReach)
  val Invariant = Conclusion(StatusCode.Invariant)
  val PreCondition = Conclusion(StatusCode.PreCondition)
  val PreConditionState = Conclusion(StatusCode.PreConditionState)
  val PostCondition = Conclusion(StatusCode.PostCondition)

  case class Strategy(
    cache: CacheStrategy = CacheStrategy.none,
    reaction: ReactionStrategy = ReactionStrategy.none
  ) {
    def toPayload = Strategy.Payload(cache.name, reaction.name)
  }
  object Strategy {
    val none = Strategy()
    val input = Strategy(reaction = ReactionStrategy.InputReaction)
    val retry = Strategy(reaction = ReactionStrategy.RetryReaction)
    val escalate = Strategy(reaction = ReactionStrategy.EscalateReaction)

    @SerialVersionUID(1L)
    case class Payload(
      cache: String,
      reaction: String
    ) {
      def restore: Strategy = {
        val c = CacheStrategy.get(cache) getOrElse CacheStrategy.NoneCache
        val r = ReactionStrategy.get(reaction) getOrElse ReactionStrategy.EscalateReaction
        Strategy(c, r)
      }
    }

    def make(faults: Faults): Strategy = {
      case class Z(reaction: DetailCode.Reaction) {
        def r = toReactionStrategy(reaction)

        def +(rhs: Fault) = copy(reaction = reaction max rhs.reaction)
      }
      faults.faults.headOption.
        map(x => faults.faults.tail.foldLeft(Z(x.reaction))(_+_).r).
        getOrElse(none)
    }

    def toReactionStrategy(p: DetailCode.Reaction) = {
      import DetailCode.Reaction._
      p.stakeholder match {
        case Client => p.action match {
          case Input => input
          case Retry => retry
          case _ => escalate
        }
        case ApplicationManager => escalate
        case ApplicationAdministrator => escalate
        case SystemAdministrator => escalate
        case SystemDeveloper => escalate
      }
    }
  }

  def apply(code: Int): Conclusion = Conclusion(StatusCode(code))

  def apply(status: StatusCode, faults: Faults): Conclusion = {
    val strategy = Strategy.make(faults)
    Conclusion(status, faults = faults, strategy = strategy)
  }

  def make(p: Throwable): Conclusion = {
    val e = ExceptionUtils.normalize(p)
    Conclusion(StatusCode.make(e), exception = Some(e))
  }

  def make(p: Throwable, label: String): Conclusion = {
    val e = ExceptionUtils.normalize(p)
    Conclusion(StatusCode.make(e), messageI18NOption = Some(I18NMessage(label)), exception = Some(e))
  }

  def make(p: NonEmptyList[Fault]): Conclusion = make(Faults(p.toList))

  def make(p: Faults): Conclusion = Conclusion(p.guessStatusCode, p)

  def error(code: Int): Conclusion = Conclusion(StatusCode(code))
  def error(code: Int, msg: Option[String]): Conclusion =
    msg.fold(error(code))(error(code, _))
  def error(code: Int, p: String): Conclusion = error(code, I18NMessage(p))
  def error(code: Int, p: I18NString): Conclusion = error(code, p.toI18NMessage)
  def error(code: Int, p: I18NMessage): Conclusion = Conclusion(
    StatusCode(code),
    Some(p)
  )
  def error(code: Int, e: Throwable): Conclusion = Conclusion(
    StatusCode(code), exception = Some(e)
  )
  def error(code: Int, msg: String, e: Throwable): Conclusion = Conclusion(
    StatusCode(code), exception = Some(e)
  )
  def error(code: Int, msg: I18NString, e: Throwable): Conclusion = Conclusion(
    StatusCode(code), Some(msg.toI18NMessage), exception = Some(e)
  )
  def error(code: Int, msg: I18NMessage, e: Throwable): Conclusion = Conclusion(
    StatusCode(code), Some(msg), exception = Some(e)
  )

  def errorData(code: Int, p: ExceptionData): Conclusion = Conclusion(StatusCode(code), exceptionData = Some(p))
  def errorData(s: StatusCode, p: ExceptionData): Conclusion = Conclusion(s, exceptionData = Some(p))

  def argumentFault(ps: Seq[ArgumentFault]): Conclusion = {
    val detail = DetailCode.Argument
    val status = StatusCode.BadRequest.withDetail(detail)
    val faults = Faults(ps)
    Conclusion(status, faults)
  }

  def resultFault(ps: Seq[ResultFault]): Conclusion = {
    val detail = DetailCode.Result
    val status = StatusCode.InternalServerError.withDetail(detail)
    val faults = Faults(ps)
    Conclusion(status, faults)
  }

  def invalidArgumentFault(message: String): Conclusion = invalidArgumentFault(I18NMessage(message))

  def invalidArgumentFault(message: I18NMessage): Conclusion = {
    val detail = DetailCode.Argument
    val status = StatusCode.BadRequest.withDetail(detail)
    val faults = Faults(InvalidArgumentFault(message))
    Conclusion(status, faults)
  }

  def missingArgumentFault(name: String, names: String*): Conclusion = missingArgumentFault(name +: names)

  def missingArgumentFault(names: Seq[String]): Conclusion = {
    val detail = DetailCode.Argument
    val status = StatusCode.BadRequest.withDetail(detail)
    val faults = Faults(MissingArgumentFault(names))
    Conclusion(status, faults)
  }

  def tooManyArgumentsFault(values: Seq[Any]): Conclusion = {
    val detail = DetailCode.Argument
    val status = StatusCode.BadRequest.withDetail(detail)
    val faults = Faults(TooManyArgumentsFault(values))
    Conclusion(status, faults)
  }

  def missingElementFault(): Conclusion = {
    val detail = DetailCode.Argument
    val status = StatusCode.BadRequest.withDetail(detail)
    val faults = Faults(MissingPropertyFault())
    Conclusion(status, faults)
  }

  def invalidPropertyFault(key: String, value: Any): Conclusion =
    invalidPropertyFault(s"$key: $value")

  def invalidPropertyFault(message: String): Conclusion = invalidPropertyFault(I18NMessage(message))

  def invalidPropertyFault(message: I18NMessage): Conclusion = {
    val detail = DetailCode.Argument
    val status = StatusCode.BadRequest.withDetail(detail)
    val faults = Faults(InvalidPropertyFault(message))
    Conclusion(status, faults)
  }

  def missingPropertyFault(name: String, names: String*): Conclusion = missingPropertyFault(name +: names)

  def missingPropertyFault(names: Seq[String]): Conclusion = {
    val detail = DetailCode.Argument
    val status = StatusCode.BadRequest.withDetail(detail)
    val faults = Faults(MissingPropertyFault(names))
    Conclusion(status, faults)
  }

  def invalidTokenFault(value: String): Conclusion = {
    val detail = DetailCode.Argument
    val status = StatusCode.BadRequest.withDetail(detail)
    val faults = Faults(InvalidTokenFault(value))
    Conclusion(status, faults)
  }

  def invalidTokenFault(label: String, value: String): Conclusion = {
    val detail = DetailCode.Argument
    val status = StatusCode.BadRequest.withDetail(detail)
    val faults = Faults(InvalidTokenFault(label, value))
    Conclusion(status, faults)
  }

  def valueDomainFault(value: String): Conclusion = {
    val detail = DetailCode.Argument
    val status = StatusCode.BadRequest.withDetail(detail)
    val faults = Faults(ValueDomainFault(value))
    Conclusion(status, faults)
  }

  def valueDomainFault(label: String, value: String): Conclusion = {
    val detail = DetailCode.Argument
    val status = StatusCode.BadRequest.withDetail(detail)
    val faults = Faults(ValueDomainFault(label, value))
    Conclusion(status, faults)
  }

  def valueDomainFault(name: String, datatype: spec.DataType, value: Any): Conclusion = {
    val detail = DetailCode.Argument
    val status = StatusCode.BadRequest.withDetail(detail)
    val faults = Faults(ValueDomainFault(name, datatype, value))
    Conclusion(status, faults)
  }

  def syntaxErrorFault(message: String): Conclusion = {
    val detail = DetailCode.Argument // TODO
    val status = StatusCode.BadRequest.withDetail(detail)
    val faults = Faults(SyntaxErrorFault(message))
    Conclusion(status, faults)
  }

  def syntaxErrorFault(messages: Seq[Message]): Conclusion = {
    val detail = DetailCode.Argument // TODO
    val status = StatusCode.BadRequest.withDetail(detail)
    val faults = Faults(SyntaxErrorFault(messages))
    Conclusion(status, faults)
  }

  def formatErrorFault(message: String): Conclusion = {
    val detail = DetailCode.Argument // TODO
    val status = StatusCode.BadRequest.withDetail(detail)
    val faults = Faults(FormatErrorFault(message))
    Conclusion(status, faults)
  }

  def formatErrorFault(messages: Seq[Message]): Conclusion = {
    val detail = DetailCode.Argument // TODO
    val status = StatusCode.BadRequest.withDetail(detail)
    val faults = Faults(FormatErrorFault(messages))
    Conclusion(status, faults)
  }

  def unmarshallingDefect(message: String): Conclusion = {
    val status = StatusCode.NoReach
    val faults = Faults(UnmarshallingDefect(message))
    Conclusion(status, faults)
  }

  def unsupportedOperationFault(message: String): Conclusion = {
    val detail = DetailCode.Argument
    val status = StatusCode.BadRequest.withDetail(detail)
    val faults = Faults(UnsupportedOperationFault(message))
    Conclusion(status, faults)
  }

  def unsupportedFormatFault(message: String): Conclusion = {
    val detail = DetailCode.Argument
    val status = StatusCode.BadRequest.withDetail(detail)
    val faults = Faults(UnsupportedFormatFault(message))
    Conclusion(status, faults)
  }

  def databaseIoFault(message: String): Conclusion = {
    val detail = DetailCode.IoDatabase
    val status = StatusCode.InternalServerError.withDetail(detail)
    val faults = Faults(DatabaseIoFault(message))
    Conclusion(status, faults)
  }

  def fileIoFault(message: String): Conclusion = {
    val detail = DetailCode.IoFile
    val status = StatusCode.InternalServerError.withDetail(detail)
    val faults = Faults(FileIoFault(message))
    Conclusion(status, faults)
  }

  def networkIoFault(message: String): Conclusion = {
    val detail = DetailCode.IoNetwork
    val status = StatusCode.InternalServerError.withDetail(detail)
    val faults = Faults(NetworkIoFault(message))
    Conclusion(status, faults)
  }

  def systemIoFault(message: String): Conclusion = {
    val detail = DetailCode.IoSystem
    val status = StatusCode.InternalServerError.withDetail(detail)
    val faults = Faults(SystemIoFault(message))
    Conclusion(status, faults)
  }

  def subsystemIoFault(message: String): Conclusion = {
    val detail = DetailCode.IoSubSystem
    val status = StatusCode.InternalServerError.withDetail(detail)
    val faults = Faults(SubsystemIoFault(message))
    Conclusion(status, faults)
  }

  def noReachDefect(message: String): Conclusion = {
    val status = StatusCode.NoReach
    val faults = Faults(NoReachDefect(message))
    Conclusion(status, faults)
  }

  object config {
    def invalidPropertyFault(key: String, value: Any): Conclusion =
      invalidPropertyFault(s"$key: ${AnyUtils.toString(value)}")

    def invalidPropertyFault(message: String): Conclusion = invalidPropertyFault(I18NMessage(message))

    def invalidPropertyFault(message: I18NMessage): Conclusion = {
      val detail = DetailCode.Config
      val status = StatusCode.InternalServerError.withDetail(detail)
      val faults = Faults(InvalidPropertyFault(message))
      Conclusion(status, faults)
    }

    def missingPropertyFault(name: String, names: String*): Conclusion = missingPropertyFault(name +: names)

    def missingPropertyFault(names: Seq[String]): Conclusion = {
      val detail = DetailCode.Config
      val status = StatusCode.InternalServerError.withDetail(detail)
      val faults = Faults(MissingPropertyFault(names))
      Conclusion(status, faults)
    }

    def invalidTokenFault(value: String): Conclusion = {
      val detail = DetailCode.Config
      val status = StatusCode.InternalServerError.withDetail(detail)
      val faults = Faults(InvalidTokenFault(value))
      Conclusion(status, faults)
    }

    def invalidTokenFault(label: String, value: String): Conclusion = {
      val detail = DetailCode.Config
      val status = StatusCode.InternalServerError.withDetail(detail)
      val faults = Faults(InvalidTokenFault(label, value))
      Conclusion(status, faults)
    }

    def valueDomainFault(value: String): Conclusion = {
      val detail = DetailCode.Config
      val status = StatusCode.InternalServerError.withDetail(detail)
      val faults = Faults(ValueDomainFault(value))
      Conclusion(status, faults)
    }

    def valueDomainFault(label: String, value: String): Conclusion = {
      val detail = DetailCode.Config
      val status = StatusCode.InternalServerError.withDetail(detail)
      val faults = Faults(ValueDomainFault(label)) // XXX
      Conclusion(status, faults)
    }

    def syntaxErrorFault(messages: Seq[Message]): Conclusion = {
      val detail = DetailCode.Config
      val status = StatusCode.InternalServerError.withDetail(detail)
      val faults = Faults(SyntaxErrorFault(messages))
      Conclusion(status, faults)
    }

    def illegalConfigurationDefect(msg: String): Conclusion = {
      val detail = DetailCode.Config
      val status = StatusCode.InternalServerError.withDetail(detail)
      val faults = Faults(IllegalConfigurationDefect(msg))
      Conclusion(status, faults)
    }

    def capacityOverflowFault(key: String, value: Any): Conclusion = {
      val msg = s"Capacity overflow: $key: ${AnyUtils.toString(value)}"
      capacityOverflowFault(msg)
    }

    def capacityOverflowFault(msg: String): Conclusion = {
      val detail = DetailCode.Config
      val status = StatusCode.InternalServerError.withDetail(detail)
      val faults = Faults(IllegalConfigurationDefect(msg))
      Conclusion(status, faults)
    }
  }

  trait ExceptionData {
    def getMessageI18N: Option[I18NMessage]
    def getIRecord: Option[IRecord] = None
    def getJson: Option[JsValue] = None
    def toPayload: ExceptionData.Payload
  }
  object ExceptionData {
    trait Payload {
      def restore: ExceptionData
    }

    sealed trait HttpBody extends ExceptionData {
    }
    object HttpBody {
      case class BodyString(body: String) extends HttpBody with Payload {
        def getMessageI18N: Option[I18NMessage] = Some(I18NMessage.create(s"HttpBody: ${Strings.cutstring(body)}"))
        def toPayload: ExceptionData.Payload = this
        def restore = this
      }

      case class BodyJson(json: JsValue) extends HttpBody with Payload {
        def getMessageI18N: Option[I18NMessage] = None
        override def getJson = Some(json)
        def toPayload: ExceptionData.Payload = this
        def restore = this
      }

      def apply(p: Any): HttpBody = p match {
        case m: String => BodyString(m)
        case m => RAISE.notImplementedYetDefect
      }
    }

    case class DataRecord(record: IRecord) extends ExceptionData {
      def getMessageI18N: Option[I18NMessage] = None
      override def getIRecord: Option[IRecord] = Some(record)
      def toPayload: ExceptionData.Payload = DataRecord.DataRecordPayload(record.toMap)
    }
    object DataRecord {
      case class DataRecordPayload(record: Map[String, Any]) extends Payload {
        def restore = DataRecord(IRecord.create(record))
      }
    }

    def apply(p: IRecord): ExceptionData = DataRecord(p)
  }

  @SerialVersionUID(1L)
  case class Payload(
    code: StatusCode.Payload,
    messageI18NOption: Option[I18NMessage.Payload],
    errors: ErrorMessages.Payload,
    warnings: WarningMessages.Payload,
    exception: Option[Payload.ExceptionPayload],
    exceptionData: Option[ExceptionData.Payload],
    faults: Faults.Payload,
    data: Option[Any],
    trace: Trace.Payload,
    strategy: Conclusion.Strategy.Payload,
    properties: Map[String, Any] = Map.empty
  ) {
    def reconstitute(): Conclusion = {
      Conclusion(
        code.restore,
        messageI18NOption.map(_.restore),
        errors.restore,
        warnings.restore,
        exception.map(_.restore),
        exceptionData.map(_.restore),
        faults.restore,
        data,
        trace.restore,
        strategy.restore
      )
    }
  }
  object Payload {
    @SerialVersionUID(1L)
    case class ExceptionPayload(e: Throwable) {
      def restore: Throwable = e
    }

    class PayloadException(o: AnyRef) extends GoldenportException(AnyUtils.toShow(o)) {
    }
  }
}
