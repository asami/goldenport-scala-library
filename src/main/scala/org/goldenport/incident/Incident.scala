package org.goldenport.incident

import scala.language.existentials
import org.goldenport.collection.NonEmptyVector
import org.goldenport.i18n.I18NString
import org.goldenport.io.Retry

/*
 * @since   Jun.  7, 2019
 *  version Jun.  9, 2019
 * @version Mar. 27, 2022
 * @author  ASAMI, Tomoharu
 */
sealed trait Incident {
  def start: Long
  def end: Long
  def message: Option[I18NString]
  def exception: Option[Throwable]
  def exceptions: Option[NonEmptyVector[Throwable]] = exception.map(x => NonEmptyVector(x))
  def exceptionVector: Vector[Throwable] = exceptions.map(_.vector).getOrElse(Vector.empty)
  def print: String = toString // TODO
}

trait SequenceIncidentImpl { self: Incident =>
  def incidents: NonEmptyVector[Incident]
  def start = incidents.head.start
  def end = incidents.last.end
  def message: Option[I18NString] = I18NString.concatOption(incidents.vector.flatMap(_.message))
  def exception = exceptions.map(_.head)
  override def exceptions = {
    val xs = incidents.vector.flatMap(_.exceptionVector)
    NonEmptyVector.createOption(xs)
  }
}

case class SequenceIncident(
  incidents: NonEmptyVector[Incident]
) extends Incident with SequenceIncidentImpl {
}

sealed trait IoIncident extends Incident {
}

case class RetrySequenceIncident(
  incidents: NonEmptyVector[Incident]
) extends IoIncident with SequenceIncidentImpl {
}

case class RetryIncident(
  start: Long,
  end: Long,
  message: Option[I18NString],
  retryCount: Int, // 0 means no retry.
  outcome: Retry.Outcome[_]
) extends IoIncident {
  def isSuccess: Boolean = outcome.isSuccess

  def exception = outcome match {
    case m: Retry.SuccessOutcome[_] => None
    case m: Retry.ValueErrorOutcome[_] => None
    case Retry.ExceptionErrorOutcome(e) => Some(e)
  }

  def isNoRetry = retryCount == 0
}

case class TraverseIncident(
  start: Long,
  end: Long,
  key: Any,
  target: Any,
  result: Option[Any],
  message: Option[I18NString],
  exception: Option[Throwable]
) extends Incident {
}

sealed trait StorageIoIncident extends IoIncident {
}

case class FileIoIncident(
  start: Long,
  end: Long,
  message: Option[I18NString],
  exception: Option[Throwable]
) extends StorageIoIncident {
}

case class DbIoIncident(
  start: Long,
  end: Long,
  message: Option[I18NString],
  exception: Option[Throwable]
) extends StorageIoIncident {
}

sealed trait NetworkIoIncident extends IoIncident {
}

case class HttpIoIncident(
  start: Long,
  end: Long,
  message: Option[I18NString],
  exception: Option[Throwable]
) extends NetworkIoIncident {
}

trait ApplicationIncident extends Incident {
}
