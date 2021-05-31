package org.goldenport.statemachine

import com.typesafe.config.{Config => Hocon, ConfigFactory, ConfigObject}
import org.goldenport.parser.ParseResult
import org.goldenport.hocon.RichConfig.Implicits._
import org.goldenport.context.Consequence

/*
 * @since   Jan.  4, 2021
 * @version May. 29, 2021
 * @author  ASAMI, Tomoharu
 */
case class StateMachineClass(
  name: String,
  rule: StateMachineRule,
  logic: StateMachineLogic
) {
  def spawn: StateMachine = {
    val sm = new StateMachine(this, logic.initState())
    sm.goAhead()
    sm
  }

  def accept(sm: StateMachine, state: State, p: Parcel): Consequence[Boolean] = logic.accept(state, p)

  def receive(sm: StateMachine, state: State, p: Parcel): Consequence[(State, Parcel)] = logic.receive(sm, state, p)

  def goAhead(sm: StateMachine, state: State): (State, Vector[StateMachine.HistorySlot]) = logic.goAhead(state)
}

object StateMachineClass {
  val PROP_STM_STATEMACHINE = "statemachine"
  val PROP_STM_NAME = "name"
  val PROP_STM_RULE = "rule"

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
