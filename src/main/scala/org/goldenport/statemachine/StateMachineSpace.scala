package org.goldenport.statemachine

import org.goldenport.RAISE
import org.goldenport.collection.PathMap
import org.goldenport.context.Consequence
import org.goldenport.event.ObjectId
import org.goldenport.event.Event

/*
 * @since   May. 20, 2021
 *  version May. 30, 2021
 *  version Jun. 13, 2021
 *  version Sep. 25, 2021
 *  version Oct. 31, 2021
 *  version Nov. 29, 2021
 * @version Sep. 30, 2026
 * @author  ASAMI, Tomoharu
 */
class StateMachineSpace(
) {
  private var _classes: PathMap[StateMachineClass] = PathMap.empty
  private var _machines: Vector[StateMachine] = Vector.empty

  def classes: PathMap[StateMachineClass] = _classes

  def addClasses(p: PathMap[StateMachineClass]): StateMachineSpace = {
    _classes = _classes + p
    this
  }

  // def issueEvent(evt: Event): Parcel = {
  //   val ctx = ExecutionContext.create()
  //   val parcel = Parcel(ctx, evt)
  //   issueEvent(parcel)
  // }

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

  def spawn(
    name: String
  )(implicit ctx: ExecutionContext): StateMachine = RAISE.notImplementedYetDefect

  def spawnOption(
    name: String
  )(implicit ctx: ExecutionContext): Option[StateMachine] = {
    classes.get(name).map(_.spawn).map(_register)
  }

  def spawnOption(
    name: String,
    to: ObjectId
  )(implicit ctx: ExecutionContext): Option[StateMachine] = {
    classes.get(name).map(_.spawn(to)).map(_register)
  }

  def register(p: StateMachine) = _register(p)

  private def _register(p: StateMachine) = synchronized {
    val a = _machines.filterNot(_.isSame(p))
    _machines = a :+ p
    p
  }
}

object StateMachineSpace {
  def create(): StateMachineSpace = new StateMachineSpace()

  def create(p: PathMap[StateMachineClass]): StateMachineSpace = create().addClasses(p)
}
