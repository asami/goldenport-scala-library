package org.goldenport.log

import org.slf4j._
import org.goldenport.value._

/*
 * @since   Sep. 16, 2018
 *  version Sep. 17, 2018
 * @version Oct.  6, 2018
 * @author  ASAMI, Tomoharu
 */
case class LogMark(
  location: LogMark.Location,
  action: LogMark.Action,
  label: String
) {
  def name = s"${location.name}.${action.name}.${label}"
  lazy val marker: Marker = MarkerFactory.getMarker(name)
}

object LogMark {
  sealed trait Location extends NamedValueInstance {
  }
  object Location extends EnumerationClass[Location] {
    val elements = Vector(
      SystemLocation,
      ExecuteLocation,
      FunctionLocation,
      ThreadLocation,
      DatabaseLocation,
      NetworkLocation
    )
  }
  case object SystemLocation extends Location {
    val name = "system"
  }
  case object ExecuteLocation extends Location {
    val name = "execute"
  }
  case object FunctionLocation extends Location {
    val name = "function"
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
}
