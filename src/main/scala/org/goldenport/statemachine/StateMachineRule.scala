package org.goldenport.statemachine

import scalaz._, Scalaz._
import org.goldenport.parser.ParseResult
import com.typesafe.config.{Config => Hocon, ConfigFactory, ConfigObject}
import org.goldenport.hocon.RichConfig.Implicits._
import org.goldenport.event.EventClazz

/*
 * @since   Jan.  4, 2021
 *  version Apr. 26, 2021
 * @version May. 27, 2021
 * @author  ASAMI, Tomoharu
 */
case class StateMachineRule(
  name: Option[String] = None,
  events: List[EventClazz] = Nil, // XXX unused
  states: List[StateClass] = Nil
) {
  def addEvents(p: List[EventClazz]) = copy(events = events ::: p)

  def initState: StateClass = states.head
}

object StateMachineRule {
  val PROP_STMRULE_STATEMACHINE = "statemachine"
  val PROP_STMRULE_EVENT = "event"
  val PROP_STMRULE_STATE = "state"
  val PROP_STMRULE_NAME = "name"
  val PROP_STMRULE_ACTIVITY = "activity"
  val PROP_STMRULE_ENTRY = "entry"
  val PROP_STMRULE_EXIT = "exit"
  val PROP_STMRULE_DO = "do"
  val PROP_STMRULE_TRANSITION = "transition"
  val PROP_STMRULE_GUARD = "guard"
  val PROP_STMRULE_TO = "to"

  case class Config()
  object Config {
    val default = Config()
  }

  def parse(p: String): ParseResult[StateMachineRule] = Builder().build(p)

  def build(p: Hocon): ParseResult[StateMachineRule] = Builder().build(p)

  def buildBody(p: Hocon): ParseResult[StateMachineRule] = Builder().buildBody(p)

  case class Builder(
    config: Config = Config.default
  ) {
    def build(rule: String): ParseResult[StateMachineRule] = {
      val hocon = ConfigFactory.parseString(rule)
      build(hocon)
    }

    def build(hocon: Hocon): ParseResult[StateMachineRule] = for {
      ev <- _events(hocon)
      stm <- _statemachine_rule(hocon)
    } yield stm.addEvents(ev)

    private def _events(p: Hocon): ParseResult[List[EventClazz]] = {
      val cs = p.takeConfigList(PROP_STMRULE_EVENT)
      cs.traverse(_event)
    }

    private def _event(p: Hocon): ParseResult[EventClazz] =
      for {
        name <- p.parseString(PROP_STMRULE_NAME)
      } yield EventClazz(name)

    private def _statemachine_rule(p: Hocon): ParseResult[StateMachineRule] = 
      p.getConfigOption(PROP_STMRULE_STATEMACHINE).
        map(buildBody).
        getOrElse(ParseResult.error(s"Missing $PROP_STMRULE_STATEMACHINE"))

    def buildBody(p: Hocon): ParseResult[StateMachineRule] =
      for {
        states <- _states(p)
      } yield StateMachineRule(None, Nil, states)

    private def _states(p: Hocon): ParseResult[List[StateClass]] = {
      val cs = p.takeConfigList(PROP_STMRULE_STATE)
      cs.traverse(_state)
    }

    private def _state(p: Hocon): ParseResult[StateClass] =
      for {
        name <- p.parseString(PROP_STMRULE_NAME)
        entrya <- _activity(p, PROP_STMRULE_ENTRY)
        exita <- _activity(p, PROP_STMRULE_EXIT)
        doa <- _do_activity(p, PROP_STMRULE_DO)
        ts <- _transition(p, PROP_STMRULE_TRANSITION)
      } yield StateClass(name, entrya, exita, doa, ts.toVector)

    private def _activity(p: Hocon, key: String): ParseResult[Activity] =
      for {
        r <- p.parseStringOrConfigOption(key)
        a <- r match {
          case Some(s) => s match {
            case Left(l) => _activity_by_name(l)
            case Right(r) => _activity_by_object(r)
          }
          case None => ParseResult(NoneActivity)
        }
      } yield a

    private def _activity_by_name(p: String): ParseResult[Activity] =
      ParseResult(NoneActivity) // TODO

    private def _activity_by_object(p: Hocon): ParseResult[Activity] =
      ParseResult(NoneActivity) // TODO

    private def _do_activity(p: Hocon, key: String): ParseResult[DoActivity] =
      ParseResult(NoneDoActivity)

    private def _transition(p: Hocon, key: String): ParseResult[List[Transition]] = {
//      println(p)
//      println(p.parseAsConfigList(key))
      p.parseAsObjectList(key, _to_transition)
    }

    private def _to_transition(p: Hocon): ParseResult[Transition] = for {
      gurad <- _guard(p)
      activity <- _activity(p, PROP_STMRULE_ACTIVITY)
      tostate <- _to_state(p)
    } yield new Transition(gurad, activity, tostate)

    private def _guard(p: Hocon): ParseResult[SmGuard] = for {
      r <- p.parseStringOrConfigOption(PROP_STMRULE_GUARD)
      a <- r match {
        case Some(s) => s match {
          case Left(l) => _guard_by_name(l)
          case Right(r) => _guard_by_object(r)
        }
        case None => ParseResult.success(AllGuard)
      }
    } yield a

    private def _guard_by_name(p: String): ParseResult[SmGuard] =
      ParseResult(EventNameGuard(p))

    private def _guard_by_object(p: Hocon): ParseResult[SmGuard] =
      ParseResult(AllGuard) // TODO

    private def _to_state(p: Hocon): ParseResult[TransitionTo] = for {
      r <- p.parseStringOrConfig(PROP_STMRULE_TO)
      a <- r match {
        case Left(l) => _to_state_by_name(l)
        case Right(r) => _to_state_by_object(r)
      }
    } yield a

    private def _to_state_by_name(p: String): ParseResult[TransitionTo] =
      ParseResult(NameTransitionTo(p))

    private def _to_state_by_object(p: Hocon): ParseResult[TransitionTo] =
      ParseResult(NoneTransitionTo) // TODO
  }
}
