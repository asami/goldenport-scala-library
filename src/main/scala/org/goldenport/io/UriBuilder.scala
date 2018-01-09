package org.goldenport.io

import scala.util.control.NonFatal
import java.net.URI
import com.asamioffice.goldenport.io.UURL
import org.goldenport.util.StringUtils

/*
 * @since   Oct.  6, 2017
 *  version Oct.  6, 2017
 *  version Dec. 20, 2017
 * @version Jan.  4, 2018
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

  def addQuery(q: Seq[(String, String)]): UriBuilder = {
    val a: String = query.map(b =>
      if (q.isEmpty)
        b
      else
        s"${b}&${_make_url_query_params(q)}"
    ).getOrElse(
      _make_url_query_params(q)
    )
    copy(query = Some(a))
  }

  private def _make_url_query_params(q: Seq[(String, String)]) = q.map {
    case (k, v) => s"${k}=${v}"
  }.mkString("&")
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
