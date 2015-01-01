package org.goldenport

import com.asamioffice.goldenport.text.UString

/**
 * derived from com.asamioffice.text.Strings(Nov. 28, 2010)
 * 
 * @since   Dec.  5, 2011
 *  version Dec.  5, 2011
 *  version Sep. 24, 2012 (move from org.goldenport)
 *  version Dec. 27, 2012
 *  version Feb.  4, 2013
 *  version Aug. 27, 2013
 *  version Oct. 23, 2013
 *  version Apr. 18, 2014
 *  version Jun.  6, 2014
 *  version Jul. 10, 2014
 * @version Dec. 21, 2014
 * @author  ASAMI, Tomoharu
 */
object Strings {
  object mimetype {
    val application_atom_xml = "application/atom+xml"
    val application_ecmascript = "application/ecmascript"
    val application_json = "application/json"
    val application_javascript = "application/javascript"
    val application_octet_stream = "application/octet-stream"
    val application_pdf = "application/pdf"
    val application_postscript = "application/postscript"
    val application_rss_xml = "application/rss+xml"
    val application_soap_xml = "application/soap+xml"
    val application_xhtml_xml = "application/xhtml+xml"
    val application_xml_dtd = "application/xml-dtd"
    val application_zip = "application/zip"
    val application_x_gzip = "application/x-gzip"
    val application_excel = "application/vnd.ms-excel"
    val image_gif = "image/gif"
    val image_jpeg = "image/jpeg"
    val image_pjpeg = "image/pjpeg"
    val image_png = "image/png"
    val image_svg_xml = "image/svg+xml"
    val image_tiff = "image/tiff"
    val image_vnd_microsoft_icon = "image/vnd.microsoft.icon"
    val message_http = "message/http"
    val message_imdn_xml = "message/imdn+xml"
    val message_partianl = "message/partial"
    val message_rfc822 = "message/rfc822"
    val multipart_mixed = "multipart/mixed"
    val multipart_alternative = "multipart/alternative"
    val multipart_related = "multipart/related"
    val multipart_form_data = "multipart/form-data"
    val multipart_signed = "multipart/signed"
    val multipart_encrypted = "multipart/encrypted"
    val text_css = "text/css"
    val text_csv = "text/csv"
    val text_html = "text/html"
    val text_plain = "text/plain"
    val text_xml = "text/xml"
    val text_event_stream = "text/event-stream"
  }

  object httpstatus {
    // 200
    val OK = "OK"
    val Created = "Created"
    val Accepted = "Accepted"
    val NonAuthoritativeInformation = "Non-Authoritative Information"
    val NoContent = "No Content"
    val ResetContent = "Reset Content"
    val PartialContent = "Partial Content"
    val MultiStatus = "Multi-Status"
    val AlreadyReported = "Already Reported"
    val IMUsed = "IM Used"
    val AuthenticationSuccessful = "Authentication Successful"
    // 400
    val BadRequest = "Bad Request"
    val Unauthorized = "Unauthorized"
    val PaymentRequired = "Payment Required"
    val Forbidden = "Forbidden"
    val NotFound = "Not Found"
    val MethodNotAllowed = "Method Not Allowed"
    val NotAcceptable = "Not Acceptable"
    val ProxyAuthenticationRequired = "Proxy Authentication Required"
    val RequestTimeout = "Request Timeout"
    val Conflict = "Conflict"
    val Gone = "Gone"
    // 500
    val InternalServerError = "Internal Server Error"
    val NotImplemented = "Not Implemented"
    val BadGateway = "Bad Gateway"
    val ServiceUnavailable = "Service Unavailable"
    val GatewayTimeout = "Gateway Timeout"

    val message = Map(
      200 -> OK,
      201 -> Created,
      202 -> Accepted,
      203 -> NonAuthoritativeInformation,
      204 -> NoContent,
      205 -> ResetContent,
      206 -> PartialContent,
      207 -> MultiStatus,
      208 -> AlreadyReported,
      226 -> IMUsed,
      230 -> AuthenticationSuccessful,
      400 -> BadRequest,
      401 -> Unauthorized,
      402 -> PaymentRequired,
      403 -> Forbidden,
      404 -> NotFound,
      405 -> MethodNotAllowed,
      406 -> NotAcceptable,
      407 -> ProxyAuthenticationRequired,
      408 -> RequestTimeout,
      409 -> Conflict,
      410 -> Gone,
      500 -> InternalServerError,
      501 -> NotImplemented,
      502 -> BadRequest,
      503 -> ServiceUnavailable,
      504 -> GatewayTimeout)

    def code(key: String): Int = {
      message.find(_._2 == key).map(_._1).get
    }
  }

  def totoken(s: String): Option[String] = {
    if (UString.isBlank(s)) None
    else Some(s.trim)
  }

  def totokens(s: String): List[String] = {
    if (UString.isBlank(s)) Nil
    else UString.getTokens(s, " ,;\t\n\r").toList
  }

  def totokens(s: String, ds: String): List[String] = {
    if (UString.isBlank(s)) Nil
    else UString.getTokens(s, ds).toList
  }

  def tolines(s: String): Vector[String] = {
    UString.getLineList(s).toVector
  }

  def tokeyvalue(s: String, ds: String = ":"): (String, String) = {
    val i = s.indexOf(ds)
    if (i == -1) (s, "")
    else (s.substring(0, i).trim, s.substring(i + 1))
  }

  def blankp(s: String): Boolean = {
    UString.isBlank(s)
  }

  def blankp(s: Option[String]): Boolean = {
    s.map(UString.isBlank) getOrElse true
  }

  def notblankp(s: String): Boolean = {
    UString.isNotBlank(s)
  }

  def notblankp(s: Option[String]): Boolean = {
    s.map(UString.isNotBlank) getOrElse false
  }

  def blankopt(s: Option[String]): Option[String] = {
    s.flatMap(x => if (blankp(x)) None else Some(x))
  }

  def cutstring(msg: String, length: Int = 1000): String = {
    if (msg.length <= length) msg
    else {
      msg.substring(0, (length - 3)) + "..."
    }
  }
}
