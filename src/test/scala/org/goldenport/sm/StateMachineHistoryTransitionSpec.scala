package org.goldenport.sm

import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/*
 * @since   Aug. 14, 2026
 * @version Aug. 14, 2026
 * @author  ASAMI, Tomoharu
 */
final class StateMachineHistoryTransitionSpec
  extends AnyWordSpec
  with Matchers
  with GivenWhenThen {

  "StateMachine named history" should {
    "resolve a named target to the composite initial direct state" in {
      Given("a machine whose Review composite has no persisted history slot")
      val review = StateMachineRule(
        name = Some("Review"),
        states = List(StateClass("Pending", 1), StateClass("Approved", 2))
      )
      val rule = StateMachineRule(
        name = Some("lifecycle"),
        states = List(StateClass("Suspended", 3)),
        statemachines = List(review)
      )
      val machine = _machine(rule, StateClass("Suspended", 3))

      When("a named Review history target is resolved")
      val transition = NamedHistoryTransitionTo("Review")
      val target = machine.historyState(transition.compositeName)

      Then("the first declared direct Review leaf is selected")
      target.rule shouldBe review
      target.state.status shouldBe "Pending"
    }

    "retain the bare history zero-arity source pattern and copy contract" in {
      Given("a newly created machine with only its initial history slot")
      val rule = StateMachineRule(states = List(StateClass("Draft", 1)))
      val machine = _machine(rule, StateClass("Draft", 1))

      When("the legacy bare history target is constructed, copied, and resolved")
      val transition = HistoryTransitionTo()
      val copied = transition.copy()
      val target = machine.historyState()

      Then("the source-compatible zero-arity pattern and current initial slot are retained")
      (transition match {
        case HistoryTransitionTo() => true
      }) shouldBe true
      copied shouldBe HistoryTransitionTo()
      target.state.status shouldBe "Draft"
    }
  }

  private def _machine(
    staterule: StateMachineRule,
    initialstate: StateClass
  ): StateMachine = {
    val logic = new StateMachineLogic {
      val rule: StateMachineRule = staterule
      def execute(sm: StateMachine, activity: Activity, p: Parcel): Parcel = {
        val _ = (sm, activity)
        p
      }
    }
    new StateMachine(StateMachineClass("lifecycle", staterule, logic), State(initialstate))
  }
}
