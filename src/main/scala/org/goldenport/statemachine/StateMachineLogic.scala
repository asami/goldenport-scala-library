package org.goldenport.statemachine

import org.goldenport.context.Consequence
import org.goldenport.event.GoAheadEvent

/*
 * @since   May. 23, 2021
 * @version May. 27, 2021
 * @author  ASAMI, Tomoharu
 */
trait StateMachineLogic {
  def rule: StateMachineRule

  def getStateClass(name: String): Option[StateClass] = rule.states.find(_.name == name)

  def initState(): State = State(rule.initState)

  def newState(p: StateClass): State = State(p)

  def accept(state: State, p: Parcel): Consequence[Boolean]

  def goAhead(state: State): State = {
    val ctx = ExecutionContext(this)
    val parcel = Parcel(ctx, GoAheadEvent)
    goAhead(state, parcel)
  }

  def goAhead(state: State, parcel: Parcel): State = {
    state.clazz.transitions.find(_.guard.isGoAhead).map(transit(state, _, parcel)).map(goAhead(_, parcel))getOrElse(state)
  }

  def entry(state: State, p: Parcel): State = {
    state.entryActivity(p) // TODO
    state.doActivity.entry(p) // TODO
    state
  }

  def transit(state: State, t: Transition, p: Parcel): State = {
    exit(state, p)
    t.activity(p) // TODO
    val s = t.to.state(p)
    entry(state, p)
    s
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
