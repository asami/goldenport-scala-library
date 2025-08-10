package org.goldenport.io

import java.io.File
import java.nio.file.{Paths, Path}
import java.net.{URL, URI}

/*
 * @since   Jun. 24, 2019
 *  version Jul. 18, 2025
 * @version Aug. 10, 2025
 * @author  ASAMI, Tomoharu
 */
sealed trait ResourceLocator {
  def getFile: Option[File]
  def getUrl: Option[URL]
  def isSame(uri: URI): Boolean
}

object ResourceLocator {
  def apply(file: File): ResourceLocator = FileResourceLocator(file)
  def apply(path: Path): ResourceLocator = PathResourceLocator(path)
  def apply(url: URL): ResourceLocator = UrlResourceLocator(url)
  def apply(uri: URI): ResourceLocator = UriResourceLocator(uri)

  def make(p: URI): ResourceLocator = {
    UriUtils.getFile(p) match {
      case Some(s) => FileResourceLocator(s)
      case None => UriResourceLocator(p)
    }
  }
}

case class FileResourceLocator(file: File) extends ResourceLocator {
  def getFile = Some(file)
  def getUrl = Some(file.toURI.toURL)
  def isSame(uri: URI): Boolean = UriUtils.getFile(uri).fold(false)(_ == file)
}

case class PathResourceLocator(path: Path) extends ResourceLocator {
  def file: File = path.toFile
  def getFile = Some(file)
  def getUrl = Some(path.toUri.toURL)
  def isSame(uri: URI): Boolean = UriUtils.getPath(uri).fold(false)(_ == path)
}

case class UrlResourceLocator(url: URL) extends ResourceLocator {
  def getFile = UrlUtils.getFile(url)
  def getUrl = Some(url)
  def isSame(uri: URI): Boolean = UriUtils.getUrl(uri).fold(false)(_ == url)
}

case class UriResourceLocator(uri: URI) extends ResourceLocator {
  def getFile = UriUtils.getFile(uri)
  def getUrl = UriUtils.getUrl(uri)
  def isSame(p: URI): Boolean = p == uri
}
