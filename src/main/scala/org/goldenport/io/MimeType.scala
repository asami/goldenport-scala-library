package org.goldenport.io

import java.io.File
import java.net.{URL, URI}
import org.goldenport.util.StringUtils

/*
 * @since   Sep. 24, 2012
 *  version Sep. 25, 2012
 *  version Sep.  2, 2017
 *  version Sep. 18, 2018
 *  version Apr. 12, 2019
 *  version Jun. 25, 2019
 * @version Aug. 18, 2019
 * @author  ASAMI, Tomoharu
 */
case class MimeType(name: String) {
  def isText: Boolean = MimeType.isText(this)
  def isBinary: Boolean = MimeType.isBinary(this)
  def isXml: Boolean = MimeType.isXml(name)
  def isHtml: Boolean = MimeType.isHtml(name)
  def isJson: Boolean = MimeType.isJson(name)
}

object MimeType {
  import org.goldenport.Strings.mimetype

  val application_atom_xml = MimeType(mimetype.application_atom_xml)
  val application_ecmascript = MimeType(mimetype.application_ecmascript)
  val application_json = MimeType(mimetype.application_json)
  val application_javascript = MimeType(mimetype.application_javascript)
  val application_octet_stream = MimeType(mimetype.application_octet_stream)
  val application_pdf = MimeType(mimetype.application_pdf)
  val application_postscript = MimeType(mimetype.application_postscript)
  val application_rss_xml = MimeType(mimetype.application_rss_xml)
  val application_soap_xml = MimeType(mimetype.application_soap_xml)
  val application_xhtml_xml = MimeType(mimetype.application_xhtml_xml)
  val application_xml_dtd = MimeType(mimetype.application_xml_dtd)
  val application_zip = MimeType(mimetype.application_zip)
  val application_x_gzip = MimeType(mimetype.application_x_gzip)
  val application_excel = MimeType(mimetype.application_excel)
  val image_gif = MimeType(mimetype.image_gif)
  val image_jpeg = MimeType(mimetype.image_jpeg)
  val image_pjpeg = MimeType(mimetype.image_pjpeg)
  val image_png = MimeType(mimetype.image_png)
  val image_svg_xml = MimeType(mimetype.image_svg_xml)
  val image_tiff = MimeType(mimetype.image_tiff)
  val image_vnd_microsoft_icon = MimeType(mimetype.image_vnd_microsoft_icon)
  val message_http = MimeType(mimetype.message_http)
  val message_imdn_xml = MimeType(mimetype.message_imdn_xml)
  val message_partianl = MimeType(mimetype.message_partianl)
  val message_rfc822 = MimeType(mimetype.message_rfc822)
  val multipart_mixed = MimeType(mimetype.multipart_mixed)
  val multipart_alternative = MimeType(mimetype.multipart_alternative)
  val multipart_related = MimeType(mimetype.multipart_related)
  val multipart_form_data = MimeType(mimetype.multipart_form_data)
  val multipart_signed = MimeType(mimetype.multipart_signed)
  val multipart_encrypted = MimeType(mimetype.multipart_encrypted)
  val text_css = MimeType(mimetype.text_css)
  val text_csv = MimeType(mimetype.text_csv)
  val text_html = MimeType(mimetype.text_html)
  val text_plain = MimeType(mimetype.text_plain)
  val text_xml = MimeType(mimetype.text_xml)
  val text_xsl = MimeType(mimetype.text_xsl)
  val text_event_stream = MimeType(mimetype.text_event_stream)

  val APPLICATION_ATOM_XML = application_atom_xml
  val APPLICATION_ECMASCRIPT = application_ecmascript
  val APPLICATION_JSON = application_json
  val APPLICATION_JAVASCRIPT = application_javascript
  val APPLICATION_OCTET_STREAM = application_octet_stream
  val APPLICATION_PDF = application_pdf
  val APPLICATION_POSTSCRIPT = application_postscript
  val APPLICATION_RSS_XML = application_rss_xml
  val APPLICATION_SOAP_XML = application_soap_xml
  val APPLICATION_XHTML_XML = application_xhtml_xml
  val APPLICATION_XML_DTD = application_xml_dtd
  val APPLICATION_ZIP = application_zip
  val APPLICATION_X_GZIP = application_x_gzip
  val APPLICATION_EXCEL = application_excel
  val IMAGE_GIF = image_gif
  val IMAGE_JPEG = image_jpeg
  val IMAGE_PJPEG = image_pjpeg
  val IMAGE_PNG = image_png
  val IMAGE_SVG_XML = image_svg_xml
  val IMAGE_TIFF = image_tiff
  val IMAGE_VND_MICROSOFT_ICON = image_vnd_microsoft_icon
  val MESSAGE_HTTP = message_http
  val MESSAGE_IMDN_XML = message_imdn_xml
  val MESSAGE_PARTIANL = message_partianl
  val MESSAGE_RFC822 = message_rfc822
  val MULTIPART_MIXED = multipart_mixed
  val MULTIPART_ALTERNATIVE = multipart_alternative
  val MULTIPART_RELATED = multipart_related
  val MULTIPART_FORM_DATA = multipart_form_data
  val MULTIPART_SIGNED = multipart_signed
  val MULTIPART_ENCRYPTED = multipart_encrypted
  val TEXT_CSS = text_css
  val TEXT_CSV = text_csv
  val TEXT_HTML = text_html
  val TEXT_PLAIN = text_plain
  val TEXT_XML = text_xml
  val TEXT_XSL = text_xsl
  val TEXT_EVENT_STREAM = text_event_stream

  val mimetypes = Vector(
    application_atom_xml,
    application_ecmascript,
    application_json,
    application_javascript,
    application_octet_stream,
    application_pdf,
    application_postscript,
    application_rss_xml,
    application_soap_xml,
    application_xhtml_xml,
    application_xml_dtd,
    application_zip,
    application_x_gzip,
    image_gif,
    image_jpeg,
    image_pjpeg,
    image_png,
    image_svg_xml,
    image_tiff,
    image_vnd_microsoft_icon,
    message_http,
    message_imdn_xml,
    message_partianl,
    message_rfc822,
    multipart_mixed,
    multipart_alternative,
    multipart_related,
    multipart_form_data,
    multipart_signed,
    multipart_encrypted,
    text_css,
    text_csv,
    text_html,
    text_plain,
    text_xml,
    text_xsl,
    text_event_stream
  ).map(x => x.name -> x).toMap

  def as(name: String): MimeType = mimetypes.get(name) getOrElse MimeType(name)

  val textMimeTypes = Vector(
    application_atom_xml,
    application_ecmascript,
    application_json,
    application_javascript
  )

  val xmlMimeTypes = Vector(
    application_atom_xml,
    application_rss_xml,
    application_soap_xml,
    application_xhtml_xml,
    image_svg_xml,
    message_imdn_xml,
    text_xml,
    text_xsl
  )

  val mimeSuffixMap = Map(
    application_json -> "json",
    application_pdf -> "pdf",
    application_postscript -> "ps",
    application_zip -> "zip",
    application_x_gzip -> "gzip",
    application_excel -> "xlsx",
    image_gif -> "gif",
    image_jpeg -> "jpeg",
    image_png -> "png",
    image_svg_xml -> "svg",
    image_tiff -> "tiff",
    text_css -> "css",
    text_csv -> "csv",
    text_html -> "html",
    text_plain -> "txt",
    text_xml -> "xml",
    text_xsl -> "xsl"
  )

  lazy val suffixMimeMap = mimeSuffixMap.map {
    case (m, s) => s -> m
  } ++ Map(
    "xls" -> application_excel
  )

  def isText(p: MimeType): Boolean = isText(p.name)
  def isText(p: String): Boolean = p.startsWith("text/") || textMimeTypes.exists(_.name == p)
  def isXml(p: String): Boolean = p.endsWith("xml") || xmlMimeTypes.exists(_.name == p)
  def isHtml(p: String): Boolean = p == mimetype.application_xhtml_xml || p.endsWith("html")
  def isJson(p: String): Boolean = p == mimetype.application_json

  def isBinary(p: MimeType): Boolean = !isText(p)
  def isBinary(p: String): Boolean = !isText(p)

  def getSuffix(p: MimeType): Option[String] = mimeSuffixMap.get(p)
  def getBySuffix(p: String): Option[MimeType] = suffixMimeMap.get(p.toLowerCase)
  def getBySuffix(p: File): Option[MimeType] = StringUtils.getSuffix(p.getPath).flatMap(getBySuffix)
  def getBySuffix(p: URL): Option[MimeType] = StringUtils.getSuffix(p.getFile).flatMap(getBySuffix)
  def getBySuffix(p: URI): Option[MimeType] = getBySuffix(p.toURL)
}
