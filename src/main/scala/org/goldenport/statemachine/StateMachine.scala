package org.goldenport.statemachine

import org.goldenport.context.Consequence
import org.goldenport.event.Event

/*
 * @since   Jan.  4, 2021
 * @version May. 27, 2021
 * @author  ASAMI, Tomoharu
 */
class StateMachine(
  clazz: StateMachineClass,
  pstate: State
) {
  private var _state: State = pstate
  def state = _state

  def accept(p: Parcel): Consequence[Boolean] = clazz.accept(this, _state, p /* .withClass(clazz) */)

  def sendPrepare(p: Parcel): Consequence[Parcel] = {
    ???
  }

  def sendCommit(p: Parcel): Consequence[Parcel] = {
    ???
  }

  def send(p: Parcel): Consequence[Parcel] = {
    ???
  }

  def goAhead(): Unit = synchronized {
    _state = clazz.goAhead(this, _state)
  }
}

object StateMachine {
}
