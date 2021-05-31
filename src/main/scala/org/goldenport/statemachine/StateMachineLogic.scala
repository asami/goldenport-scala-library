package org.goldenport.statemachine

import scalaz._, Scalaz._
import org.goldenport.trace.Result
import org.goldenport.context.Consequence
import org.goldenport.event.GoAheadEvent

/*
 * @since   May. 23, 2021
 * @version May. 30, 2021
 * @author  ASAMI, Tomoharu
 */
trait StateMachineLogic {
  def rule: StateMachineRule

  def getStateClass(name: String): Option[StateClass] = rule.states.find(_.name == name)

  def initState(): State = State(rule.initState)

  def newState(p: StateClass): State = State(p)

  def accept(state: State, p: Parcel): Consequence[Boolean] = state.accept(p)

  def receive(sm: StateMachine, state: State, p: Parcel): Consequence[(State, Parcel)] =
    for {
      str <- state.transit(p)
    } yield {
      val (s, t, r) = str
      val a = transit(state, t, r)
      //   (goAhead(a, r), r)
      (a, r)
    }

  def goAhead(state: State): (State, Vector[StateMachine.HistorySlot]) = {
    val ctx = ExecutionContext.create(this)
    val parcel = Parcel(ctx, GoAheadEvent)
    goAhead(state, parcel)
  }

  def goAhead(state: State, parcel: Parcel): (State, Vector[StateMachine.HistorySlot]) =
    state.clazz.transitions.find(_.guard.isGoAhead) match {
      case Some(s) =>
        val a = transit(state, s, parcel)
        val (b, c) = goAhead(a, parcel)
        (b, StateMachine.HistorySlot(GoAheadEvent, a) +: c)
      case None => (state, Vector.empty)
    }

  def transit(state: State, t: Transition, p: Parcel): State = {
    p.execute("StateMachineLogic#transit", state.status) {
      exit(state, p)
      t.activity(p) // TODO
      val s = t.to.state(state, p)
      entry(state, p)
      Result(s, s.status)
    }
  }

  def entry(state: State, p: Parcel): State = {
    state.entryActivity(p) // TODO
    state.doActivity.entry(p) // TODO
    state
  }

  def exit(state: State, p: Parcel): State = {
    state.doActivity.exit(p) // TODO
    state.exitActivity(p) // TODO
    state
  }
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
