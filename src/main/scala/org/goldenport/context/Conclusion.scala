package org.goldenport.context

import org.goldenport.i18n.I18NString
import org.goldenport.trace.Trace
import org.goldenport.util.ExceptionUtils

/*
 * See org.goldenport.record.v2.Conclusion.
 * 
 * @since   Feb. 21, 2021
 * @version Feb. 25, 2021
 * @author  ASAMI, Tomoharu
 */
case class Conclusion(
  code: StatusCode,
  message: Option[I18NString] = None,
  errors: ErrorMessages = ErrorMessages.empty,
  warnings: WarningMessages = WarningMessages.empty,
  exception: Option[Throwable] = None,
  faults: Faults = Faults.empty,
  effects: Effects = Effects.empty,
  trace: Option[Trace] = None
) {
  def withMessage(p: String) = copy(message = Some(I18NString(p)))
}

object Conclusion {
  val Ok = Conclusion(StatusCode.Ok)
  val BadRequest = Conclusion(StatusCode.BadRequest)
  val Unauthorized = Conclusion(StatusCode.Unauthorized)
  val NotFound = Conclusion(StatusCode.NotFound)
  val InternalServerError = Conclusion(StatusCode.InternalServerError)
  val NotImplemented = Conclusion(StatusCode.NotImplemented)

  def make(p: Throwable): Conclusion = {
    val e = ExceptionUtils.normalize(p)
    Conclusion(StatusCode.make(e), exception = Some(e))
  }

  def make(p: Throwable, label: String): Conclusion = {
    val e = ExceptionUtils.normalize(p)
    Conclusion(StatusCode.make(e), message = Some(I18NString(label)), exception = Some(e))
  }
}
