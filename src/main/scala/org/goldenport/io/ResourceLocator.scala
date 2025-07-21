package org.goldenport.io

import java.io.File
import java.net.{URL, URI}

/*
 * @since   Jun. 24, 2019
 * @version Jul. 18, 2025
 * @author  ASAMI, Tomoharu
 */
sealed trait ResourceLocator {
  def getFile: Option[File]
  def getUrl: Option[URL]
}

case class FileResourceLocator(file: File) extends ResourceLocator {
  def getFile = Some(file)
  def getUrl = Some(file.toURI.toURL)
}

case class UrlResourceLocator(url: URL) extends ResourceLocator {
  def getFile = UrlUtils.getFile(url)
  def getUrl = Some(url)
}

case class UriResourceLocator(uri: URI) extends ResourceLocator {
  def getFile = UriUtils.getFile(uri)
  def getUrl = UriUtils.getUrl(uri)
}
