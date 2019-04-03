package org.goldenport.log

import org.slf4j._
import org.goldenport.value._

/*
 * @since   Sep. 16, 2018
 *  version Sep. 17, 2018
 *  version Oct.  6, 2018
 * @version Feb. 25, 2019
 * @author  ASAMI, Tomoharu
 */
case class LogMark(
  location: LogMark.Location,
  action: LogMark.Action,
  label: Option[String]
) {
  def name = label.map(x => s"${location.name}.${action.name}.${x}").getOrElse(s"${location.name}")
  lazy val marker: Marker = MarkerFactory.getMarker(name)
}

object LogMark {
  sealed trait Location extends NamedValueInstance {
  }
  object Location extends EnumerationClass[Location] {
    val elements = Vector(
      SystemLocation,
      InterpreterLocation,
      ApplicationLocation,
      FunctionLocation,
      ThreadLocation,
      DatabaseLocation,
      NetworkLocation
    )
  }
  case object SystemLocation extends Location {
    val name = "system"
  }
  case object InterpreterLocation extends Location {
    val name = "interpreter"
  }
  case object FunctionLocation extends Location {
    val name = "function"
  }
  case object ApplicationLocation extends Location {
    val name = "application"
  }
  case object ThreadLocation extends Location {
    val name = "thread"
  }
  case object DatabaseLocation extends Location {
    val name = "database"
  }
  case object NetworkLocation extends Location {
    val name = "network"
  }
  case class ObjectLocation(o: Object) extends Location {
    val name = o.getClass.getSimpleName
  }

  sealed trait Action extends NamedValueInstance {
  }
  object Action extends EnumerationClass[Action] {
    val elements = Vector(
      ErrorAction,
      StartAction,
      EndAction,
      EndErrorAction,
      ProcessingAction
    )
  }
  case object ErrorAction extends Action {
    val name = "error"
  }
  case object StartAction extends Action {
    val name = "start"
  }
  case object EndAction extends Action {
    val name = "end"
  }
  case object EndErrorAction extends Action {
    val name = "end-error"
  }
  case object ProcessingAction extends Action {
    val name = "processing"
  }

  def apply(
    location: LogMark.Location,
    action: LogMark.Action,
    label: String
  ): LogMark = LogMark(location, action, Some(label))
}
