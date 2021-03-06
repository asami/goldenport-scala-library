package org.goldenport.statemachine

import com.typesafe.config.{Config => Hocon, ConfigFactory, ConfigObject}
import org.goldenport.parser.ParseResult
import org.goldenport.hocon.RichConfig.Implicits._
import org.goldenport.context.Consequence
import org.goldenport.statemachine.StateMachine.RuleAndState

/*
 * @since   Jan.  4, 2021
 *  version May. 29, 2021
 * @version Jun. 14, 2021
 * @author  ASAMI, Tomoharu
 */
case class StateMachineClass(
  name: String,
  rule: StateMachineRule,
  logic: StateMachineLogic
) {
  def kind = rule.kind

  def spawn: StateMachine = {
    val sm = new StateMachine(this, logic.initState())
    sm.goAhead()
    sm
  }

  def spawn(resourceid: ObjectId): StateMachine = {
    val sm = new StateMachine(this, logic.initState(), StateMachine.Content.create(resourceid))
    sm.goAhead()
    sm
  }

  def accept(sm: StateMachine, state: State, p: Parcel): Consequence[Boolean] = logic.accept(sm, state, p)

  def receive(sm: StateMachine, state: State, p: Parcel): Consequence[(RuleAndState, Parcel)] = logic.receive(sm, state, p)

  def goAhead(sm: StateMachine, state: State): (StateMachineRule, State, Vector[StateMachine.HistorySlot]) = logic.goAhead(sm, state)
}

object StateMachineClass {
  def parse(
    factory: StateMachineLogic.Factory,
    p: String
  ): ParseResult[StateMachineClass] = new Parser(factory).parse(p)

  def parseBody(
    factory: StateMachineLogic.Factory,
    name: String,
    p: String
  ): ParseResult[StateMachineClass] =  new Parser(factory).parseBody(name, p)

  class Parser(factory: StateMachineLogic.Factory) {
    def parse(p: String): ParseResult[StateMachineClass] = {
      val hocon = ConfigFactory.parseString(p)
      for {
        c <- hocon.parseConfig(PROP_STM_STATEMACHINE)
        stm <- _statemachine(c)
      } yield stm
    }

    private def _statemachine(hocon: Hocon): ParseResult[StateMachineClass] = for {
      name <- hocon.parseString(PROP_STM_NAME)
      rule <- _rule(hocon)
    } yield StateMachineClass(name, rule, factory.create(rule))

    def parseBody(name: String, p: String): ParseResult[StateMachineClass] = {
      val hocon = ConfigFactory.parseString(p)
      for {
        rule <- _rule(hocon)
      } yield StateMachineClass(name, rule, factory.create(rule))
    }

    private def _rule(hocon: Hocon): ParseResult[StateMachineRule] = for {
      sc <- hocon.parseStringOrConfigOption(PROP_STM_RULE)
      rule <- _rule(hocon, sc)
    } yield rule

    private def _rule(body: Hocon, p: Option[Either[String, Hocon]]): ParseResult[StateMachineRule] =
      p match {
        case Some(s) => s match {
          case Left(l) => _rule_by_name(l)
          case Right(r) => _rule_by_object(r)
        }
        case None => _rule_by_object(body)
      }

    private def _rule_by_name(p: String): ParseResult[StateMachineRule] =
      ParseResult.notImplemented(p)

    private def _rule_by_object(p: Hocon): ParseResult[StateMachineRule] =
      StateMachineRule.buildBody(p)
  }
}
