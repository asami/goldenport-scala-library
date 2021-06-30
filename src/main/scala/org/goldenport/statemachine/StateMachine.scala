package org.goldenport.statemachine

import scalaz._, Scalaz._
import scala.collection.immutable.Stack
import org.goldenport.context.Consequence
import org.goldenport.values.CompactUuid
import org.goldenport.event.{Event, InitEvent}
import org.goldenport.statemachine.StateMachineRule.RuleAndStateClass

/*
 * @since   Jan.  4, 2021
 *  version May. 30, 2021
 * @version Jun. 14, 2021
 * @author  ASAMI, Tomoharu
 */
class StateMachine(
  val clazz: StateMachineClass,
  initstate: State,
  val content: StateMachine.Content = StateMachine.Content.empty
) {
  import StateMachine._

  val id = CompactUuid.generateString
  def name = clazz.name
  def kind = clazz.kind
  def status = state.status

  private var _state: State = initstate
  def state = _state

  private var _statemachine_rule_stack: Stack[StateMachineRule] = Stack(clazz.rule)
  private def _current_statemachine_rule = _statemachine_rule_stack.head
  def currentStateMachineRule = _current_statemachine_rule

  def stateMacineRuleList: List[StateMachineRule] = _statemachine_rule_stack.toList

  def setCurrentStateMachineRule(p: StateMachineRule): Unit = _set_rule(p)

  private var _history: Vector[HistorySlot] = Vector(HistorySlot(InitEvent, _current_statemachine_rule, initstate))
  def history = _history

  def getStateClass(name: String): Option[RuleAndStateClass] = 
    stateMacineRuleList.flatMap(_.findState(name)).headOption

  def historyState(): RuleAndState = _history(_history.length - 2).toRuleAndState

  // TODO atomic
  def accept(p: Parcel): Consequence[Boolean] = clazz.accept(this, _state, p.withClass(clazz))

  def sendPrepare(p: Parcel): Consequence[Parcel] = Consequence.success(p)

  def sendCommit(p: Parcel): Consequence[Parcel] = synchronized {
    for {
      sp <- clazz.receive(this, _state, p.withClass(clazz))
    } yield {
      _state = sp._1.state
      _set_rule(sp._1.rule)
      _history = _history :+ HistorySlot(p.event, _current_statemachine_rule, _state)
      goAhead()
      sp._2
    }
  }

  def send(p: Parcel): Consequence[Parcel] = for {
    prepared <- sendPrepare(p)
    commited <- sendCommit(p)
  } yield commited

  def goAhead(): Unit = synchronized {
    val (c, s, h) = clazz.goAhead(this, _state)
    _state = s
    _set_rule(c)
    _history = _history ++ h
  }

  private def _set_rule(p: StateMachineRule): Unit =
    if (clazz.rule == p)
      _statemachine_rule_stack = Stack(p)
    else
      _statemachine_rule_stack = _statemachine_rule_stack.push(p) // TODO nest
}

object StateMachine {
  case class Content(
    resourceId: Option[ObjectId] = None
  )
  object Content {
    val empty = Content()

    def create(resourceid: ObjectId): Content = Content(Some(resourceid))
  }

  case class RuleAndState(rule: StateMachineRule, state: State)

  case class HistorySlot(event: Event, statemachine: StateMachineRule, state: State) {
    def toRuleAndState: RuleAndState = RuleAndState(statemachine, state)
  }
}
