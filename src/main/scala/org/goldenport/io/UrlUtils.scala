package org.goldenport.io

import scala.util.control.NonFatal
import java.net.URL
import java.net.URI
import java.io.File
import com.asamioffice.goldenport.io.UURL
import org.goldenport.util.StringUtils

/*
 * @since   Jul. 24, 2017
 *  version Aug. 30, 2017
 *  version Oct.  6, 2017
 *  version Dec.  9, 2019
 *  version Jan. 26, 2020
 *  version Mar. 21, 2022
 *  version Jul. 18, 2025
 * @version Nov.  3, 2025
 * @author  ASAMI, Tomoharu
 */
object UrlUtils {
  val urlSchemes = Set("http", "https", "file", "ftp", "file")

  def takeLeafName(url: URL): String =
    StringUtils.pathLastComponent(url.getPath)

  def takeLeafNameBody(url: URL): String =
    StringUtils.pathLastComponentBody(url.getPath)

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
      new URI(s + "/").toURL
  }

  def build(protocol: String, authority: String, path: String, query: Option[String], fragment: Option[String]): URL =
    new URI(buildString(protocol, authority, path, query, fragment)).toURL

  def buildString(protocol: String, authority: String, path: String, query: Option[String], fragment: Option[String]): String = {
    val authoritypath = StringUtils.concatPath(authority, path)
    s"""$protocol://$authoritypath${query.map("?" + _).getOrElse("")}${fragment.map("#" + _).getOrElse("")}"""
  }

  def addPathBodyPostfix(p: URL, postfix: String): URL = {
    val builder = UriBuilder(p)
    builder.addPathBodyPostfix(postfix).buildURL
  }

  def addQuery(p: URL, query: String): URL = {
    val builder = UriBuilder(p)
    builder.addQuery(query).buildURL
  }

  def getFile(url: URL): Option[File] = Option(UURL.getActiveFile(url))

  def addPath(url: URL, path: String): URL = {
    // FUTURE UriBuilder
    val baseuri = url.toURI
    val normalizedpath = if (path.startsWith("/")) path else "/" + path
    baseuri.resolve(normalizedpath).toURL
  }

  def addPathAuto(url: URL, path: String): URL = {
    val builder = UriBuilder(url)
    builder.addPathAuto(path).buildURL
  }
}
