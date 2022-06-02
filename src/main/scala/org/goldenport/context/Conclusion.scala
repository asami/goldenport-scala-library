package org.goldenport.context

import scalaz._, Scalaz._
import java.util.Locale
import org.goldenport.i18n.I18NString
import org.goldenport.i18n.I18NMessage
import org.goldenport.trace._
import org.goldenport.util.ExceptionUtils

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
 * @version May. 31, 2022
 * @author  ASAMI, Tomoharu
 */
case class Conclusion(
  code: StatusCode,
  messageI18NOption: Option[I18NMessage] = None,
  errors: ErrorMessages = ErrorMessages.empty,
  warnings: WarningMessages = WarningMessages.empty,
  exception: Option[Throwable] = None,
  faults: Faults = Faults.empty,
  trace: Trace = Trace.empty,
  strategy: Conclusion.Strategy = Conclusion.Strategy.none
) {
  def messageI18N: I18NMessage = messageI18NOption orElse _errors_message orElse _warnings_message orElse exception.map(x => I18NMessage(x.getMessage)) orElse faults.getMessageI18N getOrElse code.message

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

  def +(rhs: Conclusion): Conclusion = Conclusion(
    code,
    messageI18NOption,
    errors + rhs.errors,
    warnings + rhs.warnings,
    exception, // CAUTION
    faults, // CAUTION
    trace, // CAUTION
    strategy // CAUTION
  )

  def toException: Exception = new ConclusionException(this)
  def RAISE: Nothing = throw toException

  def isSuccess: Boolean = code.isSuccess

  def forConfig: Conclusion = if (isSuccess) this else copy(code = code.forConfig)
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
  )
  object Strategy {
    val none = Strategy()
    val input = Strategy(reaction = ReactionStrategy.InputReaction)
    val retry = Strategy(reaction = ReactionStrategy.RetryReaction)
    val escalate = Strategy(reaction = ReactionStrategy.EscalateReaction)

    def make(faults: Faults): Strategy = {
      case class Z(reaction: DetailCode.Reaction) {
        def r = toReactionStrategy(reaction)

        def +(rhs: Fault) = copy(reaction = reaction max rhs.reaction)
      }
      faults.faults.headOption.
        map(x => faults.faults.tail./:(Z(x.reaction))(_+_).r).
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

  def make(p: NonEmptyList[Fault]): Conclusion = make(Faults(p.list))

  def make(p: Faults): Conclusion = Conclusion(p.guessStatusCode, p)

  def error(code: Int, p: String): Conclusion = error(code, I18NMessage(p))
  def error(code: Int, p: I18NString): Conclusion = error(code, p.toI18NMessage)
  def error(code: Int, p: I18NMessage): Conclusion = Conclusion(
    StatusCode(code),
    Some(p)
  )
  def error(code: Int, e: Throwable): Conclusion = Conclusion(
    StatusCode(code), exception = Some(e)
  )

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

  def missingElementFault(): Conclusion = {
    val detail = DetailCode.Argument
    val status = StatusCode.BadRequest.withDetail(detail)
    val faults = Faults(MissingPropertyFault())
    Conclusion(status, faults)
  }

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
    val faults = Faults(ValueDomainFault(label)) // XXX
    Conclusion(status, faults)
  }

  def syntaxErrorFault(message: String): Conclusion = {
    val detail = DetailCode.Argument
    val status = StatusCode.BadRequest.withDetail(detail)
    val faults = Faults(SyntaxErrorFault(message))
    Conclusion(status, faults)
  }

  def syntaxErrorFault(messages: Seq[Message]): Conclusion = {
    val detail = DetailCode.Argument
    val status = StatusCode.BadRequest.withDetail(detail)
    val faults = Faults(SyntaxErrorFault(messages))
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

  object config {
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
  }
}
