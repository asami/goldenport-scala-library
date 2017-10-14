package org.goldenport.io

import scala.util.control.NonFatal
import java.net.URI
import com.asamioffice.goldenport.io.UURL
import org.goldenport.util.StringUtils

/*
 * @since   Oct.  6, 2017
 * @version Oct.  6, 2017
 * @author  ASAMI, Tomoharu
 */
case class UriBuilder(
  scheme: Option[String],
  authority: Option[String],
  path: String,
  //  query: Option[List[(String, String)]],
  query: Option[String],
  fragment: Option[String]
) {
//  def getQueryString = query.map(_.map(x => s"${x._1}=${x._2}").mkString("&"))
  def getQueryString = query
  def build: URI = new URI(
    scheme getOrElse null,
    authority getOrElse null,
    path,
    getQueryString getOrElse null,
    fragment getOrElse null
  )

  def addPath(p: String): UriBuilder = copy(path = StringUtils.concatPath(path, p))
}

object UriBuilder {
  def apply(uri: URI): UriBuilder = {
    val scheme = Option(uri.getScheme)
    val authority = Option(uri.getAuthority)
    val path = Option(uri.getPath) getOrElse ""
    val query = Option(uri.getQuery)
    val fragment = Option(uri.getFragment)
    UriBuilder(scheme, authority, path, query, fragment)
  }
}
