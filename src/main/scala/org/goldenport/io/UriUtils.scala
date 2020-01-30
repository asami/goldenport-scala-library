package org.goldenport.io

import scala.util.control.NonFatal
import java.net.{URI, URL}
import java.io.File
import com.asamioffice.goldenport.io.UURL
import org.goldenport.values.Urn

/*
 * @since   Oct.  6, 2017
 *  version Apr. 26, 2019
 * @version Jan. 26, 2020
 * @author  ASAMI, Tomoharu
 */
object UriUtils {
  def addPath(uri: URI, path: String) = UriBuilder(uri).addPath(path).build
  def sibling(uri: URI): URI = UriBuilder.byPath("..").addPath(uri).build

  def getFile(uri: URI): Option[File] =
    if (uri.getScheme == "file")
      Some(new File(uri.toURL.getFile))
    else
      None

  def getUrl(uri: URI): Option[URL] =
    if (UrlUtils.urlSchemes.contains(uri.getScheme))
      Some(uri.toURL)
    else
      None

  def getUrn(uri: URI): Option[Urn] =
    if (UrlUtils.urlSchemes.contains(uri.getScheme))
      None
    else
      Some(Urn(uri.toString))
}
