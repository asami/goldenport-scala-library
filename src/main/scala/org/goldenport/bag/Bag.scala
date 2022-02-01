package org.goldenport.bag

import java.io.{OutputStream, Writer}
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.io.MimeType
import org.goldenport.extension.IRecord
import scalax.io.Codec

/*
 * @since   Sep. 27, 2015
 *  version Oct. 24, 2015
 *  version Sep. 22, 2016
 *  version Aug. 29, 2017
 *  version May. 19, 2020
 * @version Feb.  1, 2022
 * @author  ASAMI, Tomoharu
 */
trait Bag {
  def name: String = "data"
  def filename: String = filenameSuffix match {
    case Some(s) => s"${filenameBody}.${s}"
    case None => filenameBody
  }
  def filenameBody: String = name
  def filenameSuffix: Option[String] = Some("bin")
//  def encoding: String = "UTF-8"
  protected final def default_mimetype = Nature.defaultMimetype
  protected final def suffix_to_mimetype(suffix: String): MimeType = MimeType.getBySuffix(suffix) getOrElse default_mimetype
  protected final def suffix_to_mimetype(suffix: Option[String]): MimeType = suffix.map(suffix_to_mimetype) getOrElse default_mimetype
  def mimetype: MimeType = default_mimetype
  def getCodec: Option[Codec] = None
//  def weakCodec: Codec = getCodec getOrElse Codec.UTF8
  def properties: IRecord = IRecord.empty

  def getChunkBag: Option[ChunkBag]
  def createChunkBag: ChunkBag

  def withName(p: String): ChunkBag = RAISE.unsupportedOperationFault
  def withSuffix(p: String): ChunkBag = RAISE.unsupportedOperationFault
  def withFilename(p: String): ChunkBag = RAISE.unsupportedOperationFault
  def withMimetype(p: MimeType): ChunkBag = RAISE.unsupportedOperationFault
  def withCodec(p: Codec): ChunkBag = RAISE.unsupportedOperationFault
  def withProperties(p: IRecord): ChunkBag = RAISE.unsupportedOperationFault

  def copyTo(out: OutputStream) { // not closing (i.e. ZipOutputStream)
    getChunkBag match {
      case Some(s) => s.copyTo(out)
      case None =>
        for (bag <- resource.managed(createChunkBag)) {
          bag.copyTo(out)
        }
    }
  }
  def copyTo(out: Writer) { // not closing (i.e. ZipOutputStream)
    getChunkBag match {
      case Some(s) => s.copyTo(out)
      case None =>
        for (bag <- resource.managed(createChunkBag)) {
          bag.copyTo(out)
        }
    }
  }
}

object Bag {
  case class Designation(
    name: String,
    suffix: Option[String],
    mimetype: MimeType,
    codec: Option[Codec] = None,
    properties: IRecord = IRecord.empty
  )
  object Designation {
    trait Impl extends Bag {
      def designation: Designation
      def update(p: Designation): ChunkBag

      override def name: String = designation.name
      override def filenameSuffix: Option[String] = designation.suffix
      override def mimetype: MimeType = designation.mimetype
      override def getCodec: Option[Codec] = designation.codec

      override def withName(p: String): ChunkBag = update(designation.copy(name = p))
      override def withSuffix(p: String): ChunkBag = update(designation.copy(suffix = Some(p)))
      override def withMimetype(p: MimeType): ChunkBag = update(designation.copy(mimetype = p))
      override def withCodec(p: Codec): ChunkBag = update(designation.copy(codec = Some(p)))
      override def withProperties(p: IRecord): ChunkBag = update(designation.copy(properties = p))
    }

    val default = Designation("data", Some("bin"), Nature.defaultMimetype)

    def create(name: String, suffix: String, mimetype: MimeType): Designation =
      Designation(name, Some(suffix), mimetype)
  }
}
