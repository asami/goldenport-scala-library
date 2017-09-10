package org.goldenport.bag

import java.io.{OutputStream, Writer}
import org.goldenport.Strings
import scalax.io.Codec

/*
 * @since   Sep. 27, 2015
 *  version Oct. 24, 2015
 *  version Sep. 22, 2016
 * @version Aug. 29, 2017
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
  def mimetype: String = Strings.mimetype.application_octet_stream
  def getCodec: Option[Codec] = None
//  def weakCodec: Codec = getCodec getOrElse Codec.UTF8
  def getChunkBag: Option[ChunkBag]
  def createChunkBag: ChunkBag
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
