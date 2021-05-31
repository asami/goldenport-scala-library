package org.goldenport.context

import org.goldenport.Strings
import org.goldenport.i18n.I18NString
import org.goldenport.util.ExceptionUtils

/*
 * @since   Feb. 20, 2021
 *  version Feb. 21, 2021
 *  version Mar. 26, 2021
 *  version Apr. 10, 2021
 * @version May. 30, 2021
 * @author  ASAMI, Tomoharu
 */
case class StatusCode(
  main: Int,
  detail: Option[DetailCode] = None,
  application: Option[Int] = None, // FURTHER CONSIDERATION
  messageOption: Option[I18NString] = None, // FURTHER CONSIDERATION
  externalService: Option[StatusCode] = None // FURTHER CONSIDERATION
) {
  def message: I18NString = messageOption getOrElse StatusCode.message(main)

  def withDetail(p: DetailCode) = copy(detail = Some(p))
}

object StatusCode {
  /*
   * Main
   */
  final val Ok = StatusCode(200)
  final val Created = StatusCode(201)
  final val Accepted = StatusCode(202)
  final val NonAuthoritativeInformation = StatusCode(203)
  final val NoContent = StatusCode(204)
  final val ResetContent = StatusCode(205)
  final val PartialContent = StatusCode(206)
  final val MultiStatus = StatusCode(207)
  final val AlreadyReported = StatusCode(208)
  final val IMUsed = StatusCode(226)
  final val AuthenticationSuccessful = StatusCode(230)
  final val MultipleChoices = StatusCode(300)
  final val MovedPermanently = StatusCode(301)
  final val Found = StatusCode(302)
  final val SeeOther = StatusCode(303)
  final val NotModified = StatusCode(304)
  final val UseProxy = StatusCode(305)
  final val SwitchProxy = StatusCode(306)
  final val TemporaryRedirect = StatusCode(307)
  final val PermanentRedirect = StatusCode(308)
  final val BadRequest = StatusCode(400)
  final val Unauthorized = StatusCode(401)
  final val PaymentRequired = StatusCode(402)
  final val Forbidden = StatusCode(403)
  final val NotFound = StatusCode(404)
  final val MethodNotAllowed = StatusCode(405)
  final val NotAcceptable = StatusCode(406)
  final val ProxyAuthenticationRequired = StatusCode(407)
  final val RequestTimeout = StatusCode(408)
  final val Conflict = StatusCode(409)
  final val Gone = StatusCode(410)
  final val InternalServerError = StatusCode(500)
  final val NotImplemented = StatusCode(501)
  final val BadGateway = StatusCode(502)
  final val ServiceUnavailable = StatusCode(503)
  final val GatewayTimeout = StatusCode(504)

  /*
   * Main and Detail
   */
  final val IllegalArugument = BadRequest // TODO
  final val SyntaxError = BadRequest // TODO
  final val NoReach = InternalServerError.withDetail(DetailCode.NoReach)
  final val Invariant = InternalServerError.withDetail(DetailCode.Invariant)
  final val PreCondition = InternalServerError.withDetail(DetailCode.PreCondition)
  final val PreConditionState = InternalServerError.withDetail(DetailCode.PreConditionState)
  final val PostCondition = InternalServerError.withDetail(DetailCode.PostCondition)

  def message(code: Int): I18NString = {
    val a = Strings.httpstatus.take(code)
    I18NString(a)
  }

  /*
   * Function
   */
  def make(p: Throwable): StatusCode = {
    val e = ExceptionUtils.normalize(p)
    e match {
      case _ => InternalServerError
    }
  }
}
