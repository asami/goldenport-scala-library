package org.goldenport.io

import java.io._
import java.net.{URL, URI}
import java.nio.charset.Charset
import scalax.io.Codec
import org.goldenport.RAISE

/*
 * See InputSource
 *
 * @since   Aug. 17, 2019
 *  version Aug. 18, 2019
 * @version Dec.  8, 2019
 * @author  ASAMI, Tomoharu
 */
trait ResourceHandle {
  def name: String
  def getMimeType: Option[MimeType]
  def openInputStream(): InputStream
  def openOutputStream(): OutputStream = RAISE.unsupportedOperationFault(s"Read only resource: $name")
  def asText: String = IoUtils.toText(this)
  def asText(encoding: String): String = IoUtils.toText(this, encoding)
  def asText(charset: Option[Charset]): String = IoUtils.toText(this, charset)
  def asText(charset: Charset): String = IoUtils.toText(this, charset)
  def asText(codec: Codec): String = IoUtils.toText(this, codec)
}

class FileResourceHandle(
  manager: ResourceManager,
  file: File,
  mimeTypeOption: Option[MimeType] = None
) extends ResourceHandle {
  override def toString() = s"File:$file"

  def name = file.toString
  def getMimeType = mimeTypeOption orElse MimeType.getBySuffix(file)
  def openInputStream = new FileInputStream(file)
}

class UrlResourceHandle(
  manager: ResourceManager,
  url: URL,
  mimeTypeOption: Option[MimeType] = None
) extends ResourceHandle {
  override def toString() = s"URL:$url"

  def name = url.toString
  def getMimeType = mimeTypeOption orElse MimeType.getBySuffix(url)
  def openInputStream = url.openStream
}

class UriResourceHandle(
  manager: ResourceManager,
  uri: URI,
  mimeTypeOption: Option[MimeType] = None
) extends ResourceHandle {
  override def toString() = s"URI:$uri"

  lazy val url: URL = uri.toURL
  def name = uri.toString
  def getMimeType = mimeTypeOption orElse MimeType.getBySuffix(uri)
  def openInputStream = url.openStream
}
