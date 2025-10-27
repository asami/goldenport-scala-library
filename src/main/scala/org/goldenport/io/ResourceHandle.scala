package org.goldenport.io

import java.io._
import java.net.{URL, URI}
import java.nio.charset.Charset
import scalax.io.Codec
import com.asamioffice.goldenport.io.UURL
import org.goldenport.RAISE
import org.goldenport.context.Conclusion
import org.goldenport.bag.ChunkBag

/*
 * See InputSource
 *
 * @since   Aug. 17, 2019
 *  version Aug. 18, 2019
 *  version Dec.  8, 2019
 *  version Mar.  6, 2021
 * @version Oct. 24, 2025
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
  def url: URL
}

object ResourceHandle {
  def create(locator: ResourceLocator, bag: ChunkBag): ResourceHandle =
    new LocatorBagResourceHandle(locator, bag)
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
  def url = UURL.getURLFromFile(file)
}

class UrlResourceHandle(
  manager: ResourceManager,
  val url: URL,
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

class LocatorBagResourceHandle(
  locator: ResourceLocator,
  bag: ChunkBag
) extends ResourceHandle {
  def url = locator.getUrl getOrElse Conclusion.illegalStateDefect(s"no URL in $this").RAISE
  def name = url.toString
  def getMimeType = MimeType.getBySuffix(url)
  def openInputStream() = bag.openInputStream
}
