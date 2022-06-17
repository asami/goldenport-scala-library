package org.goldenport.context

import org.goldenport.Strings
import org.goldenport.i18n.I18NString
import org.goldenport.i18n.I18NMessage
import org.goldenport.util.ExceptionUtils

/*
 * @since   Feb. 20, 2021
 *  version Feb. 21, 2021
 *  version Mar. 26, 2021
 *  version Apr. 10, 2021
 *  version May. 30, 2021
 *  version Jun. 20, 2021
 *  version Jan. 20, 2022
 *  version Mar.  6, 2022
 * @version Jun. 13, 2022
 * @author  ASAMI, Tomoharu
 */
case class StatusCode(
  code: Int,
  detail: Option[DetailCode] = None,
  application: Option[Int] = None, // FURTHER CONSIDERATION
  messageOption: Option[I18NMessage] = None, // FURTHER CONSIDERATION
  externalService: Option[StatusCode] = None // FURTHER CONSIDERATION
) {
  import StatusCode._

  def message: I18NMessage = messageOption getOrElse StatusCode.message(code)

  def withDetail(p: DetailCode) = copy(detail = Some(p))

  def isSuccess: Boolean = Ok.code <= code && code < MultipleChoices.code

  def forConfig: StatusCode =
    if (isSuccess)
      this
    else
      copy(code = InternalServerError.code, detail = detail.map(_.forConfig))

  def toPayload: StatusCode.Payload = StatusCode.Payload(
    code,
    detail.map(_.toPayload),
    application,
    messageOption.map(_.toPayload),
    externalService.map(_.toPayload)
  )
}

object StatusCode {
  /*
   * Code
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
   * Code and Detail
   */
  final val IllegalArugument = BadRequest.withDetail(DetailCode.Argument)
  final val SyntaxError = BadRequest.withDetail(DetailCode.ArgumentSyntax)
  final val Config = InternalServerError.withDetail(DetailCode.Config)
  final val NoReach = InternalServerError.withDetail(DetailCode.NoReach)
  final val Invariant = InternalServerError.withDetail(DetailCode.Invariant)
  final val PreCondition = InternalServerError.withDetail(DetailCode.PreCondition)
  final val PreConditionState = InternalServerError.withDetail(DetailCode.PreConditionState)
  final val PostCondition = InternalServerError.withDetail(DetailCode.PostCondition)

  @SerialVersionUID(1L)
  case class Payload(
    code: Int,
    detail: Option[DetailCode.Payload],
    application: Option[Int],
    messageOption: Option[I18NMessage.Payload],
    externalService: Option[StatusCode.Payload]
  ) {
    def restore: StatusCode = StatusCode(
      code,
      detail.map(_.restore),
      application,
      messageOption.map(_.restore),
      externalService.map(_.restore)
    )
  }

  def message(code: Int): I18NMessage = {
    val a = Strings.httpstatus.take(code)
    I18NMessage(a)
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
