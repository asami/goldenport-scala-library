package org.goldenport.io

import scala.util.control.NonFatal
import java.net.URL
import com.asamioffice.goldenport.io.UURL
import org.goldenport.util.StringUtils

/*
 * @since   Jul. 24, 2017
 *  version Aug. 30, 2017
 *  version Oct.  6, 2017
 * @version Dec.  9, 2019
 * @author  ASAMI, Tomoharu
 */
object UrlUtils {
  def takeLeafName(url: URL): String =
    StringUtils.pathLastComponent(url.getPath)

  def isExist(url: URL): Boolean =
    Option(UURL.getActiveFile(url)).fold {
      try {
        for (in <- resource.managed(url.openStream())) {
          in.read() // try read one byte
        }
        true
      } catch {
        case NonFatal(e) => false
      }
    } { file =>
      file.exists
    }

  def normalizeBaseUrl(p: URL): URL = {
    val s = p.toExternalForm
    if (s.endsWith("/"))
      p
    else
      new URL(s + "/")
  }

  def build(protocol: String, authority: String, path: String, query: Option[String], fragment: Option[String]): URL =
    new URL(buildString(protocol, authority, path, query, fragment))

  def buildString(protocol: String, authority: String, path: String, query: Option[String], fragment: Option[String]): String =
    s"""$protocol://$authority$path${query.map("?" + _).getOrElse("")}${fragment.map("#" + _).getOrElse("")}"""

  def addPathBodyPostfix(p: URL, postfix: String): URL = {
    val builder = UriBuilder(p)
    builder.addPathBodyPostfix(postfix).buildURL
  }

  def addQuery(p: URL, query: String): URL = {
    val builder = UriBuilder(p)
    builder.addQuery(query).buildURL
  }
}
