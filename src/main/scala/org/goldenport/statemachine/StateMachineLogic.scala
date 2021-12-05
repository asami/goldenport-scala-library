package org.goldenport.statemachine

import scalaz._, Scalaz._
import org.goldenport.trace.Result
import org.goldenport.context.Consequence
import org.goldenport.event.GoAheadEvent
import org.goldenport.event.CallEvent
import org.goldenport.statemachine.StateMachine.RuleAndState
import org.goldenport.statemachine.StateMachineRule.RuleAndStateClass

/*
 * @since   May. 23, 2021
 *  version May. 30, 2021
 *  version Jun. 14, 2021
 *  version Sep. 26, 2021
 *  version Oct. 29, 2021
 *  version Nov. 29, 2021
 * @version Dec.  5, 2021
 * @author  ASAMI, Tomoharu
 */
trait StateMachineLogic {
  def rule: StateMachineRule

//  def getStateClass(name: String): Option[StateClass] = rule.findState(name)

  def initState(): State = State(rule.initState)

  def newState(p: StateClass): State = State(p)

  def reconstitute(p: Int): Consequence[RuleAndStateClass] =
    Consequence.successOrMissingPropertyFault(p.toString, rule.findState(p))

  def reconstitute(p: String): Consequence[RuleAndStateClass] =
    Consequence.successOrMissingPropertyFault(p, rule.findState(p))

  // def reconstitute(p: Int): Consequence[State] =
  //   Consequence.successOrMissingPropertyFault(p.toString, rule.findState(p).map(x => State(x.state)))

  // def reconstitute(p: String): Consequence[State] =
  //   Consequence.successOrMissingPropertyFault(p, rule.findState(p).map(x => State(x.state)))

  def accept(sm: StateMachine, state: State, p: Parcel): Consequence[Boolean] = for {
    f <- _force_accept(sm, state, p)
    r <- f.map(Consequence.success).getOrElse {
      for {
        a <- state.accept(p)
        b <- if (a) {
          Consequence.success(true)
        } else {
          sm.stateMacineRuleList.traverse(_.transitions.accept(p)).map(_.contains(true))
        }
      } yield b
    }
  } yield r

  private def _force_accept(sm: StateMachine, state: State, p: Parcel): Consequence[Option[Boolean]] =
    Consequence(
      sm.kind match {
        case StateMachineKind.Resource => sm.content.resourceId.flatMap(rid =>
          p.event match {
            case m: CallEvent => Some(m.to.isMatch(rid))
            case _ => None
          }
        )
        case _ => None
      }
    )

  def receive(sm: StateMachine, state: State, p: Parcel): Consequence[(RuleAndState, Parcel)] =
    for {
      stro <- state.transit(sm, p)
      str <- stro match {
        case Some(s) => Consequence.success(s)
        case None =>
          val a: Consequence[(State, Transition, Parcel)] =
            sm.stateMacineRuleList.toStream.
              traverse(_.transitions.transit(sm, state, p)).
              flatMap(_.flatten.headOption match {
                case Some(s) => Consequence.success(s)
                case None => Consequence.illegalConfigurationDefect("StateMachineLogic#receive")
              })
          a
      }
    } yield {
      val (s, t, r) = str
      // val a = transit(sm, state, t, r)
      // //   (goAhead(a, r), r)
      // (a, r)
      (RuleAndState(sm.currentStateMachineRule, s), r)
    }

  def init(
    sm: StateMachine,
    state: State
  )(implicit ctx: ExecutionContext): Unit = {
    val parcel = Parcel(ctx, GoAheadEvent)
    entry(sm, state, parcel)
  }

  def goAhead(
    sm: StateMachine,
    state: State
  )(implicit ctx: ExecutionContext): (StateMachineRule, State, Vector[StateMachine.HistorySlot]) = {
//    val ctx = ExecutionContext.create(this)
    val parcel = Parcel(ctx, GoAheadEvent)
    goAhead(sm, state, parcel)
  }

  def goAhead(sm: StateMachine, state: State, parcel: Parcel): (StateMachineRule, State, Vector[StateMachine.HistorySlot]) =
    state.clazz.transitions.findGoAhead(parcel) match {
      case Some(s) =>
        val a = transit(sm, state, s, parcel)
        val (c, b, h) = goAhead(sm, a.state, parcel)
        (c, b, StateMachine.HistorySlot(GoAheadEvent, sm.currentStateMachineRule, a.state) +: h)
      case None => (sm.currentStateMachineRule, state, Vector.empty)
    }

  def transit(sm: StateMachine, state: State, t: Transition, p: Parcel): RuleAndState = {
    p.execute("StateMachineLogic#transit", state.status) {
      val rs = t.to.state(sm, state, p)
      sm.setCurrentStateMachineRule(rs.rule)
      exit(sm, state, p)
      t.effect(sm, p) // TODO parcel
      entry(sm, rs.state, p)
      Result(rs, rs.state.status)
    }
  }

  def entry(sm: StateMachine, state: State, p: Parcel): State = {
    state.entryActivity(sm, p) // TODO parcel
    state.doActivity.entry(sm, p) // TODO parcel
    state
  }

  def exit(sm: StateMachine, state: State, p: Parcel): State = {
    state.doActivity.exit(sm, p) // TODO parcel
    state.exitActivity(sm, p) // TODO parcel
    state
  }

  def execute(sm: StateMachine, activity: Activity, p: Parcel): Parcel
}

object StateMachineLogic {
  // val none = new StateMachineLogic {
  //   def initState(): State = ???
  //   def accept(state: State, p: Parcel): Consequence[Boolean] = ???
  // }

  trait Factory {
    def create(rule: StateMachineRule): StateMachineLogic
  }
}
