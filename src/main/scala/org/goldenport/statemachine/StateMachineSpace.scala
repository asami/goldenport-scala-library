package org.goldenport.statemachine

import org.goldenport.RAISE
import org.goldenport.collection.TreeMap
import org.goldenport.context.Consequence
import org.goldenport.event.Event

/*
 * @since   May. 20, 2021
 * @version May. 25, 2021
 * @author  ASAMI, Tomoharu
 */
class StateMachineSpace(
  classes: TreeMap[StateMachineClass] = TreeMap.empty
) {
  private var _machines: Vector[StateMachine] = Vector.empty

  def issueEvent(evt: Event): Parcel = {
    val ctx = ExecutionContext()
    val parcel = Parcel(ctx, evt)
    issueEvent(parcel)
  }

  def issueEvent(parcel: Parcel): Parcel = {
    case class Z(ms: Vector[StateMachine] = Vector.empty) {
      def r = {
        _machines.map(_.sendCommit(parcel))
        parcel
      }

      def +(rhs: StateMachine) = rhs.accept(parcel) match {
        case Consequence.Success(b, _) =>
          if (b) {
            rhs.sendPrepare(parcel) match {
              case Consequence.Success(s, c) => ???
              case Consequence.Error(c) => ???
            }
          } else {
            this
          }
        case Consequence.Error(c) => ???
      }
    }
    _machines./:(Z())(_+_).r
  }

  def spawn(name: String): StateMachine = RAISE.notImplementedYetDefect

  def spawnOption(name: String): Option[StateMachine] = {
    classes.get(name).map(_.spawn).map(_register)
  }

  private def _register(p: StateMachine) = synchronized {
    _machines = _machines :+ p
    p
  }
}

object StateMachineSpace {
  def create(): StateMachineSpace = new StateMachineSpace()

  def create(p: TreeMap[StateMachineClass]): StateMachineSpace = new StateMachineSpace(p)
}
