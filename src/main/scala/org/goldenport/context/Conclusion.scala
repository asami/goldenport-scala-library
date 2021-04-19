package org.goldenport.context

import scalaz._, Scalaz._
import org.goldenport.i18n.I18NString
import org.goldenport.trace._
import org.goldenport.util.ExceptionUtils

/*
 * See org.goldenport.record.v2.Conclusion.
 * 
 * @since   Feb. 21, 2021
 *  version Feb. 25, 2021
 *  version Mar. 26, 2021
 * @version Apr. 10, 2021
 * @author  ASAMI, Tomoharu
 */
case class Conclusion(
  code: StatusCode,
  message: Option[I18NString] = None,
  errors: ErrorMessages = ErrorMessages.empty,
  warnings: WarningMessages = WarningMessages.empty,
  exception: Option[Throwable] = None,
  faults: Faults = Faults.empty,
  trace: Trace = Trace.empty,
  strategy: Conclusion.Strategy = Conclusion.Strategy.none
) {
  // def incidents: Incidents = trace.incidents
  // def faults: Faults = incidents.faults
  // def effects: Effects = incidents.effects
  // def statictics: Statictics = incidents.statictics

  def withMessage(p: String) = copy(message = Some(I18NString(p)))
  def withTrace(p: TraceHandle): Conclusion = withTrace(p.ctx)
  def withTrace(p: TraceContext): Conclusion = withTrace(p.toTrace)
  def withTrace(p: Trace): Conclusion = copy(trace = p)
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
    Conclusion(StatusCode.make(e), message = Some(I18NString(label)), exception = Some(e))
  }

  def argumentFault(ps: Seq[ArgumentFault]): Conclusion = {
    val detail = DetailCode.Argument
    val status = StatusCode.BadRequest.withDetail(detail)
    val faults = Faults(ps)
    Conclusion(status, faults)
  }
}
