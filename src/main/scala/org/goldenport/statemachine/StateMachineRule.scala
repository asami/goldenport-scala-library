package org.goldenport.statemachine

import scalaz._, Scalaz._
import com.typesafe.config.{Config => Hocon, ConfigFactory, ConfigObject}
import org.goldenport.parser.ParseResult
import org.goldenport.values.PathName
import org.goldenport.hocon.RichConfig.Implicits._
import org.goldenport.event.EventClazz

/*
 * @since   Jan.  4, 2021
 *  version Apr. 26, 2021
 *  version May. 29, 2021
 * @version Jun. 29, 2021
 * @author  ASAMI, Tomoharu
 */
case class StateMachineRule(
  name: Option[String] = None,
  parentPath: Option[PathName] = None,
  kind: StateMachineKind = StateMachineKind.Plain,
  events: List[EventClazz] = Nil, // XXX unused
  states: List[StateClass] = Nil,
  statemachines: List[StateMachineRule] = Nil,
  transitions: Transitions = Transitions.empty
) {
  import StateMachineRule.RuleAndStateClass

  def isMatch(p: String): Boolean = name == Some(p)

  def addEvents(p: List[EventClazz]) = copy(events = events ::: p)

  def initState: StateClass = states.head

  def findState(name: String): Option[RuleAndStateClass] = {
    val pn = PathName(name, ".")
    _find_state(pn.components)
  }

  private def _find_state(name: String): Option[RuleAndStateClass] =
    states.find(_.name == name).map(x => RuleAndStateClass(this, x)) orElse (
      statemachines.find(_.isMatch(name)).map(x => RuleAndStateClass(x, x.initState))
    )

  private def _find_state(sm: String, path: List[String]): Option[RuleAndStateClass] =
    statemachines.find(_.isMatch(sm)).flatMap(_._find_state(path))

  private def _find_state(path: List[String]): Option[RuleAndStateClass] = path match {
    case Nil => None
    case x :: Nil => _find_state(x)
    case x :: xs => _find_state(x, xs)
  }
}

object StateMachineRule {
  case class Config()
  object Config {
    val default = Config()
  }

  case class RuleAndStateClass(rule: StateMachineRule, state: StateClass)

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

    def buildBody(p: Hocon): ParseResult[StateMachineRule] = _statemachine(None, p)

    private def _statemachine(parent: Option[PathName], p: Hocon): ParseResult[StateMachineRule] = {
      for {
        name <- p.parseStringOption(PROP_STMRULE_NAME)
        kind <- _kind(p)
        smpathname = _statemachine_pathname(parent, name)
        states <- _states(smpathname, p)
        sms <- _statemachines(smpathname, p)
        ts <- _transition(p, PROP_STMRULE_TRANSITION)
      } yield StateMachineRule(name, parent, kind, Nil, states, sms, ts)
    }

    private def _kind(p: Hocon): ParseResult[StateMachineKind] = for {
      a <- p.parseStringOption(PROP_STMRULE_KIND)
      b <- a.map(StateMachineKind.parse).getOrElse(ParseResult.success(StateMachineKind.Plain))
    } yield b

    private def _statemachine_pathname(ancestor: Option[PathName], parent: Option[String])=
      (ancestor, parent) match {
        case (None, None) => None
        case (Some(l), None) => Some(l)
        case (None, Some(r)) => Some(PathName(r, "."))
        case (Some(l), Some(r)) => Some(l :+ r)
      }

    private def _states(smpath: Option[PathName], p: Hocon): ParseResult[List[StateClass]] = {
      val cs = p.takeConfigList(PROP_STMRULE_STATE)
      cs.traverse(_state(smpath, _))
    }

    private def _state(smpath: Option[PathName], p: Hocon): ParseResult[StateClass] =
      for {
        name <- p.parseString(PROP_STMRULE_NAME)
        ts <- _transition(p, PROP_STMRULE_TRANSITION)
        entrya <- _activity(p, PROP_STMRULE_ENTRY)
        exita <- _activity(p, PROP_STMRULE_EXIT)
        doa <- _do_activity(p, PROP_STMRULE_DO)
      } yield StateClass(name, smpath, ts, entrya, exita, doa)

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

    private def _transition(p: Hocon, key: String): ParseResult[Transitions] =
//      println(p)
//      println(p.parseAsConfigList(key))
      for {
        xs <- p.parseAsObjectList(key, _to_transition)
      } yield Transitions(xs.toVector)

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
      p match {
        case PROP_STATE_FINAL => ParseResult(FinalTransitionTo)
        case PROP_STATE_HISTORY => ParseResult(HistoryTransitionTo())
        case _ => ParseResult(NameTransitionTo(p))
      }

    private def _to_state_by_object(p: Hocon): ParseResult[TransitionTo] =
      ParseResult(NoneTransitionTo) // TODO

    // private def _statemachines(
    //   ancestor: Option[PathName],
    //   parent: Option[String],
    //   p: Hocon
    // ): ParseResult[List[StateMachineRule]] = {
    //   val pn = (ancestor, parent) match {
    //     case (None, None) => None
    //     case (Some(l), None) => Some(l)
    //     case (None, Some(r)) => Some(PathName(r, "."))
    //     case (Some(l), Some(r)) => Some(l :+ r)
    //   }
    //   _statemachines(pn, p)
    // }

    private def _statemachines(parent: Option[PathName], p: Hocon): ParseResult[List[StateMachineRule]] = {
      val cs = p.asConfigList(PROP_STMRULE_STATEMACHINE)
      cs.traverse(_statemachine(parent, _))
    }
  }
}
