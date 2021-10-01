package org.goldenport.statemachine

import org.goldenport.RAISE
import org.goldenport.collection.TreeMap
import org.goldenport.context.Consequence
import org.goldenport.event.Event

/*
 * @since   May. 20, 2021
 *  version May. 30, 2021
 *  version Jun. 13, 2021
 * @version Sep. 25, 2021
 * @author  ASAMI, Tomoharu
 */
class StateMachineSpace(
) {
  private var _classes: TreeMap[StateMachineClass] = TreeMap.empty
  private var _machines: Vector[StateMachine] = Vector.empty

  def classes: TreeMap[StateMachineClass] = _classes

  def addClasses(p: TreeMap[StateMachineClass]): StateMachineSpace = {
    _classes = _classes + p
    this
  }

  def issueEvent(evt: Event): Parcel = {
    val ctx = ExecutionContext.create()
    val parcel = Parcel(ctx, evt)
    issueEvent(parcel)
  }

  def issueEvent(parcel: Parcel): Parcel = {
    case class Z(ms: Vector[StateMachine] = Vector.empty) {
      def r = {
        ms.map(_.sendCommit(parcel)) // TODO failure
        parcel
      }

      def +(rhs: StateMachine) = rhs.accept(parcel) match {
        case Consequence.Success(b, _) =>
          if (b) {
            rhs.sendPrepare(parcel) match {
              case Consequence.Success(s, c) => copy(ms = ms :+ rhs)
              case m: Consequence.Error[_] => m.RAISE
            }
          } else {
            this
          }
        case m: Consequence.Error[_] => m.RAISE
      }
    }
    _machines./:(Z())(_+_).r
  }

  def spawn(name: String): StateMachine = RAISE.notImplementedYetDefect

  def spawnOption(name: String): Option[StateMachine] = {
    classes.get(name).map(_.spawn).map(_register)
  }

  def spawnOption(name: String, to: ObjectId): Option[StateMachine] = {
    classes.get(name).map(_.spawn(to)).map(_register)
  }

  def register(p: StateMachine) = _register(p)

  private def _register(p: StateMachine) = synchronized {
    _machines = _machines :+ p
    p
  }
}

object StateMachineSpace {
  def create(): StateMachineSpace = new StateMachineSpace()

  def create(p: TreeMap[StateMachineClass]): StateMachineSpace = create().addClasses(p)
}
