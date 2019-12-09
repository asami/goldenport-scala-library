package org.goldenport.io

import java.io.File
import java.net.{URL, URI}
import org.goldenport.util.StringUtils

/*
 * @since   Aug. 17, 2019
 *  version Aug. 18, 2019
 * @version Dec.  8, 2019
 * @author  ASAMI, Tomoharu
 */
class ResourceManager(
  suffixMimeMappings: Map[String, MimeType] = Map.empty
) {
  // TODO something uri resource mapping

  def getMimeType(url: URL): Option[MimeType] = getMimeTypeByName(url.toExternalForm)

  def getMimeType(uri: URI): Option[MimeType] = getMimeTypeByName(uri.toString)

  def getMimeType(file: File): Option[MimeType] = getMimeTypeByName(file.getName)

  def getMimeTypeByName(name: String): Option[MimeType] = StringUtils.getSuffix(name).flatMap(getMimeTypeBySuffix)

  def getMimeTypeBySuffix(suffix: String): Option[MimeType] =
    suffixMimeMappings.get(suffix) orElse MimeType.getBySuffix(suffix)

  def takeHandle(p: ResourceLocator): ResourceHandle = p match {
    case m: FileResourceLocator => takeHandle(m.file)
    case m: UrlResourceLocator => takeHandle(m.url)
    case m: UriResourceLocator => takeHandle(m.uri)
  }

  def takeHandle(file: File): ResourceHandle =
    new FileResourceHandle(this, file, getMimeType(file))

  def takeHandle(url: URL): ResourceHandle =
    new UrlResourceHandle(this, url, getMimeType(url))

  def takeHandle(uri: URI): ResourceHandle =
    (uri.getScheme, Option(uri.getHost), Option(uri.getSchemeSpecificPart)) match {
      case ("file", None, Some(s)) => takeHandle(new File(s))
      case ("http", _, Some(s)) => takeHandle(uri.toURL)
      case ("https", _, Some(s)) => takeHandle(uri.toURL)
      case (null, _, Some(s)) => takeHandle(new File(s))
      case _ => new UriResourceHandle(this, uri, getMimeType(uri))
    }
}
