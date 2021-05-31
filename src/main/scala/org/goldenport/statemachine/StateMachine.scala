package org.goldenport.statemachine

import scalaz._, Scalaz._
import org.goldenport.context.Consequence
import org.goldenport.event.{Event, InitEvent}

/*
 * @since   Jan.  4, 2021
 * @version May. 30, 2021
 * @author  ASAMI, Tomoharu
 */
class StateMachine(
  clazz: StateMachineClass,
  initstate: State
) {
  import StateMachine._

  def name = clazz.name
  def status = state.status

  private var _state: State = initstate
  def state = _state

  private var _history: Vector[HistorySlot] = Vector(HistorySlot(InitEvent, initstate))
  def history = _history

  // TODO atomic
  def accept(p: Parcel): Consequence[Boolean] = clazz.accept(this, _state, p.withClass(clazz))

  def sendPrepare(p: Parcel): Consequence[Parcel] = Consequence.success(p)

  def sendCommit(p: Parcel): Consequence[Parcel] = synchronized {
    for {
      sp <- clazz.receive(this, _state, p.withClass(clazz))
    } yield {
      _state = sp._1
      _history = _history :+ HistorySlot(p.event, _state)
      goAhead()
      sp._2
    }
  }

  def send(p: Parcel): Consequence[Parcel] = for {
    prepared <- sendPrepare(p)
    commited <- sendCommit(p)
  } yield commited

  def goAhead(): Unit = synchronized {
    val (s, h) = clazz.goAhead(this, _state)
    _state = s
    _history = _history ++ h
  }
}

object StateMachine {
  case class HistorySlot(event: Event, state: State)
}
