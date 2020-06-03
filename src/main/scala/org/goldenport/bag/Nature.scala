package org.goldenport.bag

import java.io.{OutputStream, Writer}
import scalax.io.Codec
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.io.MimeType
import org.goldenport.extension.IRecord
import org.goldenport.util.StringUtils

/*
 * @since   May. 19, 2020
 * @version May. 19, 2020
 * @author  ASAMI, Tomoharu
 */
case class Nature(
  name: String = "data",
  filenameSuffix: Option[String] = Some("bin"),
  mimetype: MimeType = MimeType.application_octet_stream,
  getCodec: Option[Codec] = None,
  properties: IRecord = IRecord.empty
) {
  def withName(p: String) = copy(name = p)

  def withSuffix(p: String) = copy(
    filenameSuffix = Some(p),
    mimetype = MimeType.getBySuffix(p) getOrElse mimetype
  )

  def withFilename(p: String) = {
    val (name, suffix) = StringUtils.pathnameBodySuffix(p)
    suffix.map(s =>
      copy(
        name = name,
        filenameSuffix = suffix,
        mimetype = Nature.suffixToMimetype(suffix)
      )
    ).getOrElse(
      copy(
        name = p,
        filenameSuffix = None,
        mimetype = Nature.defaultMimetype
      )
    )
  }

  def withMimetype(p: MimeType) = copy(mimetype = p)

  def withNameSuffix(name: String, suffix: Option[String]) =
    copy(name = name, filenameSuffix = suffix, mimetype = Nature.suffixToMimetype(suffix))

  def withNameSuffixMimetype(name: String, suffix: Option[String], mimetype: MimeType) =
    copy(name = name, filenameSuffix = suffix, mimetype = mimetype)

  def withCodec(p: Codec) = copy(getCodec = Some(p))

  def withProperties(p: IRecord) = copy(properties = p)
}

object Nature {
  val default = Nature()

  val defaultMimetype = MimeType.application_octet_stream

  def apply(p: Codec): Nature = default.withCodec(p)

  def suffixToMimetype(suffix: String): MimeType = MimeType.getBySuffix(suffix) getOrElse defaultMimetype

  def suffixToMimetype(suffix: Option[String]): MimeType = suffix.map(suffixToMimetype) getOrElse defaultMimetype

  trait Holder extends Bag {
    def nature: Nature

    override def name: String = nature.name
    override def filenameSuffix: Option[String] = nature.filenameSuffix
    override def mimetype: MimeType = nature.mimetype
    override def getCodec: Option[Codec] = nature.getCodec
    override def properties: IRecord = nature.properties
  }
}
