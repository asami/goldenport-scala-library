package org.goldenport.io

import java.io.File
import java.net.{URL, URI}

/*
 * @since   Aug. 17, 2019
 * @version Aug. 18, 2019
 * @author  ASAMI, Tomoharu
 */
class ResourceManager() {
  // TODO something uri resource mapping

  def takeHandle(p: ResourceLocator): ResourceHandle = p match {
    case m: FileResourceLocator => takeHandle(m.file)
    case m: UrlResourceLocator => takeHandle(m.url)
    case m: UriResourceLocator => takeHandle(m.uri)
  }

  def takeHandle(file: File): ResourceHandle = new FileResourceHandle(this, file)

  def takeHandle(url: URL): ResourceHandle = new UrlResourceHandle(this, url)

  def takeHandle(uri: URI): ResourceHandle =
    (uri.getScheme, Option(uri.getHost), Option(uri.getSchemeSpecificPart)) match {
      case ("file", None, Some(s)) => takeHandle(new File(s))
      case _ => new UriResourceHandle(this, uri)
    }
}
