package org.goldenport.io

/*
 * @since   Sep. 24, 2012
 *  version Sep. 25, 2012
 *  version Sep.  2, 2017
 *  version Sep. 18, 2018
 * @version Apr. 12, 2019
 * @author  ASAMI, Tomoharu
 */
case class MimeType(name: String) {
  def isText: Boolean = MimeType.isText(this)
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
  val text_event_stream = MimeType(mimetype.text_event_stream)

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
    text_event_stream
  ).map(x => x.name -> x).toMap

  def as(name: String): MimeType = mimetypes.get(name) getOrElse MimeType(name)

  val textMimeTypes = Vector(
    application_atom_xml,
    application_ecmascript,
    application_json,
    application_javascript
  )

  def isText(p: MimeType): Boolean = isText(p.name)
  def isText(p: String): Boolean = p.startsWith("text/") || textMimeTypes.exists(_.name == p)
  def isXml(p: String): Boolean = p.endsWith("xml")
  def isHtml(p: String): Boolean = p == mimetype.application_xhtml_xml || p.endsWith("html")
  def isJson(p: String): Boolean = p == mimetype.application_json
}
