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
 *  version Aug. 18, 2019
 *  version Dec.  7, 2019
 *  version Sep.  1, 2020
 *  version Feb.  8, 2021
 *  version Mar.  6, 2021
 *  version Apr. 15, 2021
 * @version Mar. 30, 2022
 * @author  ASAMI, Tomoharu
 */
case class MimeType(name: String) {
  def isText: Boolean = MimeType.isText(this)
  def isBinary: Boolean = MimeType.isBinary(this)
  def isXml: Boolean = MimeType.isXml(name)
  def isHtml: Boolean = MimeType.isHtml(name)
  def isJson: Boolean = MimeType.isJson(name)
  def isImage: Boolean = MimeType.isImage(name)
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
  val application_x_www_form_urlencoded = MimeType(mimetype.application_x_www_form_urlencoded)
  val application_excel = MimeType(mimetype.application_excel)
  val application_excelx = MimeType(mimetype.application_excelx)
  val application_powerpoint = MimeType(mimetype.application_powerpoint)
  val application_powerpointx = MimeType(mimetype.application_powerpointx)
  val application_word = MimeType(mimetype.application_word)
  val application_wordx = MimeType(mimetype.application_wordx)
  val image_gif = MimeType(mimetype.image_gif)
  val image_jpeg = MimeType(mimetype.image_jpeg)
  val image_pjpeg = MimeType(mimetype.image_pjpeg)
  val image_png = MimeType(mimetype.image_png)
  val image_svg_xml = MimeType(mimetype.image_svg_xml)
  val image_tiff = MimeType(mimetype.image_tiff)
  val image_vnd_microsoft_icon = MimeType(mimetype.image_vnd_microsoft_icon)
  val image_apng = MimeType(mimetype.image_apng)
  val image_webp = MimeType(mimetype.image_webp)
  val image_avif = MimeType(mimetype.image_avif)
  val image_xicon = MimeType(mimetype.image_xicon) // https://stackoverflow.com/questions/13827325/correct-mime-type-for-favicon-ico
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
  val text_html = MimeType(mimetype.text_html)
  val text_plain = MimeType(mimetype.text_plain)
  val text_xml = MimeType(mimetype.text_xml)
  val text_xsl = MimeType(mimetype.text_xsl)
  val text_event_stream = MimeType(mimetype.text_event_stream)
  val text_csv = MimeType(mimetype.text_csv)
  val text_tsv = MimeType(mimetype.text_tsv)
  val text_xsv = MimeType(mimetype.text_xsv)
  val text_lcsv = MimeType(mimetype.text_lcsv)
  val text_ltsv = MimeType(mimetype.text_ltsv)
  val text_lxsv = MimeType(mimetype.text_lxsv)

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
  val APPLICATION_X_WWW_FORM_URLENCODED = application_x_www_form_urlencoded
  val APPLICATION_EXCEL = application_excel
  val APPLICATION_EXCELX = application_excelx
  val APPLICATION_POWERPOINT = application_powerpoint
  val APPLICATION_POWERPOINTX = application_powerpointx
  val APPLICATION_WORD = application_word
  val APPLICATION_WORDX = application_wordx
  val IMAGE_GIF = image_gif
  val IMAGE_JPEG = image_jpeg
  val IMAGE_PJPEG = image_pjpeg
  val IMAGE_PNG = image_png
  val IMAGE_SVG_XML = image_svg_xml
  val IMAGE_TIFF = image_tiff
  val IMAGE_VND_MICROSOFT_ICON = image_vnd_microsoft_icon
  val IMAGE_APNG = image_apng
  val IMAGE_WEBP = image_webp
  val IMAGE_AVIF = image_avif
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
  val TEXT_HTML = text_html
  val TEXT_PLAIN = text_plain
  val TEXT_XML = text_xml
  val TEXT_XSL = text_xsl
  val TEXT_EVENT_STREAM = text_event_stream
  val TEXT_CSV = text_csv
  val TEXT_TSV = text_tsv
  val TEXT_XSV = text_xsv
  val TEXT_LCSV = text_lcsv
  val TEXT_LTSV = text_ltsv
  val TEXT_LXSV = text_lxsv

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
    application_excel,
    application_excelx,
    application_powerpoint,
    application_powerpointx,
    application_word,
    application_wordx,
    image_gif,
    image_jpeg,
    image_pjpeg,
    image_png,
    image_svg_xml,
    image_tiff,
    image_vnd_microsoft_icon,
    image_apng,
    image_webp,
    image_avif,
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
    text_tsv,
    text_xsv,
    text_lcsv,
    text_ltsv,
    text_lxsv,
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
    application_excel -> "xls",
    application_excelx -> "xlsx",
    application_powerpoint -> "ppt",
    application_powerpointx -> "pptx",
    application_word -> "doc",
    application_wordx -> "docx",
    image_gif -> "gif",
    image_jpeg -> "jpeg",
    image_png -> "png",
    image_svg_xml -> "svg",
    image_tiff -> "tiff",
    image_apng -> "apng",
    image_webp -> "webp",
    image_avif -> "avif",
    text_csv -> "csv",
    text_tsv -> "tsv",
    text_xsv -> "xsv",
    text_lcsv -> "lcsv",
    text_ltsv -> "ltsv",
    text_lxsv -> "lxsv",
    text_css -> "css",
    text_html -> "html",
    text_plain -> "txt",
    text_xml -> "xml",
    text_xsl -> "xsl"
  )

  lazy val suffixMimeMap = mimeSuffixMap.map {
    case (m, s) => s -> m
  } ++ Map(
    "htm" -> text_html
  )

  def isText(p: MimeType): Boolean = isText(p.name)
  def isText(p: String): Boolean = p.startsWith("text/") || textMimeTypes.exists(_.name == p)
  def isXml(p: String): Boolean = p.endsWith("xml") || xmlMimeTypes.exists(_.name == p)
  def isHtml(p: String): Boolean = p == mimetype.application_xhtml_xml || p.endsWith("html")
  def isJson(p: String): Boolean = p == mimetype.application_json
  def isImage(p: String): Boolean = p.startsWith("image/")
  def isImageFile(p: URI): Boolean = getBySuffix(p).map(_.isImage).getOrElse(false)
  def isImageFile(p: String): Boolean = getBySuffix(p).map(_.isImage).getOrElse(false)

  def isBinary(p: MimeType): Boolean = !isText(p)
  def isBinary(p: String): Boolean = !isText(p)

  def getSuffix(p: MimeType): Option[String] = mimeSuffixMap.get(p)
  def getBySuffix(p: String): Option[MimeType] = suffixMimeMap.get(p.toLowerCase)
  def getByFilename(p: String): Option[MimeType] = StringUtils.getSuffix(p).flatMap(getBySuffix)
  def getBySuffix(p: File): Option[MimeType] = StringUtils.getSuffix(p.getPath).flatMap(getBySuffix)
  def getBySuffix(p: URL): Option[MimeType] = StringUtils.getSuffix(p.getFile).flatMap(getBySuffix)
  def getBySuffix(p: URI): Option[MimeType] = StringUtils.getSuffix(p.getPath).flatMap(getBySuffix)

  object name {
    val APPLICATION_ATOM_XML = application_atom_xml.name
    val APPLICATION_ECMASCRIPT = application_ecmascript.name
    val APPLICATION_JSON = application_json.name
    val APPLICATION_JAVASCRIPT = application_javascript.name
    val APPLICATION_OCTET_STREAM = application_octet_stream.name
    val APPLICATION_PDF = application_pdf.name
    val APPLICATION_POSTSCRIPT = application_postscript.name
    val APPLICATION_RSS_XML = application_rss_xml.name
    val APPLICATION_SOAP_XML = application_soap_xml.name
    val APPLICATION_XHTML_XML = application_xhtml_xml.name
    val APPLICATION_XML_DTD = application_xml_dtd.name
    val APPLICATION_ZIP = application_zip.name
    val APPLICATION_X_GZIP = application_x_gzip.name
    val APPLICATION_X_WWW_FORM_URLENCODED = application_x_www_form_urlencoded.name
    val APPLICATION_EXCEL = application_excel.name
    val APPLICATION_EXCELX = application_excelx.name
    val APPLICATION_POWERPOINT = application_powerpoint.name
    val APPLICATION_POWERPOINTX = application_powerpointx.name
    val APPLICATION_WORD = application_word.name
    val APPLICATION_WORDX = application_wordx.name
    val IMAGE_GIF = image_gif.name
    val IMAGE_JPEG = image_jpeg.name
    val IMAGE_PJPEG = image_pjpeg.name
    val IMAGE_PNG = image_png.name
    val IMAGE_SVG_XML = image_svg_xml.name
    val IMAGE_TIFF = image_tiff.name
    val IMAGE_VND_MICROSOFT_ICON = image_vnd_microsoft_icon.name
    val IMAGE_APNG = image_apng.name
    val IMAGE_WEBP = image_webp.name
    val IMAGE_AVIF = image_avif.name
    val MESSAGE_HTTP = message_http.name
    val MESSAGE_IMDN_XML = message_imdn_xml.name
    val MESSAGE_PARTIANL = message_partianl.name
    val MESSAGE_RFC822 = message_rfc822.name
    val MULTIPART_MIXED = multipart_mixed.name
    val MULTIPART_ALTERNATIVE = multipart_alternative.name
    val MULTIPART_RELATED = multipart_related.name
    val MULTIPART_FORM_DATA = multipart_form_data.name
    val MULTIPART_SIGNED = multipart_signed.name
    val MULTIPART_ENCRYPTED = multipart_encrypted.name
    val TEXT_CSS = text_css.name
    val TEXT_HTML = text_html.name
    val TEXT_PLAIN = text_plain.name
    val TEXT_XML = text_xml.name
    val TEXT_XSL = text_xsl.name
    val TEXT_EVENT_STREAM = text_event_stream.name
    val TEXT_CSV = text_csv.name
    val TEXT_TSV = text_tsv.name
    val TEXT_XSV = text_xsv.name
    val TEXT_LCSV = text_lcsv.name
    val TEXT_LTSV = text_ltsv.name
    val TEXT_LXSV = text_lxsv.name
  }
}
