package org.goldenport

import com.asamioffice.goldenport.text.UString

/**
 * derived from com.asamioffice.text.Strings(Nov. 28, 2010)
 * 
 * @since   Dec.  5, 2011
 *  version Sep. 24, 2012 (move from org.goldenport)
 *  version Dec. 27, 2012
 *  version Feb.  4, 2013
 *  version Aug. 27, 2013
 *  version Oct. 23, 2013
 *  version Apr. 18, 2014
 *  version Jun.  6, 2014
 *  version Jul. 10, 2014
 *  version Dec. 21, 2014
 *  version Jan. 14, 2015
 *  version Oct. 25, 2015
 *  version Mar. 10, 2016
 *  version Mar. 21, 2017
 *  version Dec. 10, 2018
 *  version Jun. 24, 2019
 *  version Jul.  7, 2019
 *  version Dec.  7, 2019
 *  version Sep.  8, 2020
 *  version Apr. 15, 2021
 *  version May. 30, 2021
 *  version Mar. 30, 2022
 *  version Apr. 18, 2023
 * @version May. 30, 2024
 * @author  ASAMI, Tomoharu
 */
object Strings {
  object mimetype {
    // Use MimeType
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
    val application_x_www_form_urlencoded = "application/x-www-form-urlencoded"
    val application_excel = "application/vnd.ms-excel"
    val application_excelx = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    val application_powerpoint = "application/vnd.ms-powerpoint"
    val application_powerpointx = "application/vnd.openxmlformats-officedocument.presentationml.presentation"
    val application_word = "application/msword"
    val application_wordx = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
    val image_gif = "image/gif"
    val image_jpeg = "image/jpeg"
    val image_pjpeg = "image/pjpeg"
    val image_png = "image/png"
    val image_svg_xml = "image/svg+xml"
    val image_tiff = "image/tiff"
    val image_vnd_microsoft_icon = "image/vnd.microsoft.icon"
    val image_apng = "image/apng"
    val image_webp = "image/webp"
    val image_avif = "image/avif"
    val image_xicon = "image/x-icon"
    val audio_aac = "audio/aac"
    val audio_avi = "audio/x-msvideo"
    val audio_cda = "application/x-cdf"
    val audio_midi = "audio/midi" // x-midi
    val audio_mp3 = "audio/mpeg"
    val audio_oga = "audio/ogg"
    val audio_opus = "audio/opus"
    val audio_wav = "audio/wav"
    val audio_weba = "audio/weba"
    val audio_3gp = "audio/3gpp"
    val audio_3g2 = "audio/3gp2"
    val video_avi = "video/x-msvideo"
    val video_mp4 = "video/mp4"
    val video_mpeg = "video/mpeg"
    val video_ogv = "video/ogg"
    val video_ts = "video/mp2t"
    val video_webm = "video/webm"
    val video_3gp = "video/3gpp"
    val video_3g2 = "video/3gp2"
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
    val text_html = "text/html"
    val text_plain = "text/plain"
    val text_xml = "text/xml"
    val text_xsl = "text/xsl"
    val text_event_stream = "text/event-stream"
    val text_csv = "text/csv"
    val text_tsv = "text/tsv"
    val text_xsv = "text/xsv"
    val text_lcsv = "text/lcsv"
    val text_ltsv = "text/ltsv"
    val text_lxsv = "text/lxsv"
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
    // 300
    val MultipleChoices = "Multiple Choices"
    val MovedPermanently = "Moved Permanently"
    val Found = "Found" // Moved temporarily
    val SeeOther = "See Other"
    val NotModified = "Mot Modified"
    val UseProxy = "Use Proxy"
    val SwitchProxy = "Switch Proxy"
    val TemporaryRedirect = "Temporary Redirect"
    val PermanentRedirect = "Permanent Redirect"
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
      300 -> MultipleChoices,
      301 -> MovedPermanently,
      302 -> Found,
      303 -> SeeOther,
      304 -> NotModified,
      305 -> UseProxy,
      306 -> SwitchProxy,
      307 -> TemporaryRedirect,
      308 -> PermanentRedirect,
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
      502 -> BadGateway,
      503 -> ServiceUnavailable,
      504 -> GatewayTimeout)

    def code(key: String): Int = {
      message.find(_._2 == key).map(_._1).get
    }

    def take(code: Int): String = message.get(code) getOrElse InternalServerError
  }

  val delimiter = " ,;\t\n\r"

  def delimiterp(c: Character): Boolean = {
    delimiterp(c, delimiter)
  }

  def delimiterp(c: Character, ds: String): Boolean = {
    ds.exists(_ == c)
  }

  def totoken(s: String): Option[String] = {
    if (blankp(s)) None
    else Some(s.trim)
  }

  def totokens(s: String): List[String] = {
    if (blankp(s)) Nil
    else UString.getTokens(s, delimiter).toList
  }

  def totokens(s: String, ds: String): List[String] = {
    if (blankp(s)) Nil
    else UString.getTokens(s, ds).toList
  }

  def totokensVector(s: String, ds: String): Vector[String] =
    if (blankp(s))
      Vector.empty
    else
      UString.getTokens(s, ds).toVector

  def tolines(s: String): Vector[String] = {
    UString.getLineList(s).toVector
  }

  def tokeyvalue(s: String, ds: String = ":"): (String, String) = {
    val i = s.indexOf(ds)
    if (i == -1) (s, "")
    else (s.substring(0, i).trim, s.substring(i + 1))
  }

  def emptyp(s: String): Boolean = s == null || s.length == 0

  def emptyp(s: Option[String]): Boolean = {
    s.map(emptyp) getOrElse true
  }

  def notemptyp(s: String): Boolean = !emptyp(s)

  def notemptyp(s: Option[String]): Boolean = {
    s.map(notemptyp) getOrElse false
  }

  def emptyopt(s: String): Option[String] =
    if (emptyp(s)) None else Some(s)

  def emptyopt(s: Option[String]): Option[String] = {
    s.flatMap(x => if (emptyp(x)) None else Some(x))
  }

  def blankp(s: String): Boolean =
    s == null || s.length == 0 || s.forall(x => x == ' ' || x == '\t')

  def blankp(s: Option[String]): Boolean = {
    s.map(blankp) getOrElse true
  }

  def notblankp(s: String): Boolean = !blankp(s)

  def notblankp(s: Option[String]): Boolean = {
    s.map(notblankp) getOrElse false
  }

  def blankopt(s: String): Option[String] =
    if (blankp(s)) None else Some(s)

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
