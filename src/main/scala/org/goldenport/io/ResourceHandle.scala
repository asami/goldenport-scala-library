package org.goldenport.io

import java.io._
import java.net.{URL, URI}
import org.goldenport.RAISE

/*
 * @since   Aug. 17, 2019
 * @version Aug. 18, 2019
 * @author  ASAMI, Tomoharu
 */
trait ResourceHandle {
  def name: String
  def getMimeType: Option[MimeType]
  def openInputStream(): InputStream
  def openOutputStream(): OutputStream = RAISE.unsupportedOperationFault(s"Read only resource: $name")
}

class FileResourceHandle(
  manager: ResourceManager,
  file: File,
  mimeTypeOption: Option[MimeType] = None
) extends ResourceHandle {
  def name = file.toString
  def getMimeType = mimeTypeOption orElse MimeType.getBySuffix(file)
  def openInputStream = new FileInputStream(file)
}

class UrlResourceHandle(
  manager: ResourceManager,
  url: URL,
  mimeTypeOption: Option[MimeType] = None
) extends ResourceHandle {
  def name = url.toString
  def getMimeType = mimeTypeOption orElse MimeType.getBySuffix(url)
  def openInputStream = url.openStream
}

class UriResourceHandle(
  manager: ResourceManager,
  uri: URI,
  mimeTypeOption: Option[MimeType] = None
) extends ResourceHandle {
  lazy val url: URL = uri.toURL
  def name = uri.toString
  def getMimeType = mimeTypeOption orElse MimeType.getBySuffix(uri)
  def openInputStream = url.openStream
}
