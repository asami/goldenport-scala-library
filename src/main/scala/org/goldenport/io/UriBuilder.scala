package org.goldenport.io

import scala.util.control.NonFatal
import java.net.{URI, URL}
import com.asamioffice.goldenport.io.UURL
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.util.StringUtils

/*
 * @since   Oct.  6, 2017
 *  version Dec. 20, 2017
 *  version Jan. 14, 2018
 *  version Apr. 26, 2019
 *  version Dec.  9, 2019
 * @version Jun.  6, 2020
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

  def buildURL: URL = {
    val proto = scheme getOrElse RAISE.illegalStateFault("No protocol")
    val auth = authority getOrElse RAISE.illegalStateFault("No authority")
    UrlUtils.build(proto, auth, path, query, fragment)
  }

  def withPath(p: String) = copy(path = p)

  def withQuery(p: String): UriBuilder = {
    val v = if (Strings.blankp(p)) Some(p) else None
    copy(query = v)
  }

  def withQuery(p: Option[String]): UriBuilder = copy(query = p)

  def withQuery(p: Map[String, String]): UriBuilder = withQuery(p.toVector)

  def withQuery(p: Seq[(String, String)]): UriBuilder = withQuery(_make_url_query_params(p))

  def addPath(p: URI): UriBuilder = addPath(p.getPath)

  def addPath(p: String): UriBuilder = copy(path = StringUtils.concatPath(path, p))

  def addPathBodyPostfix(p: String): UriBuilder = {
    val (body, suffix) = StringUtils.pathnameBodySuffix(path)
    withPath(s"""${body}${p}${suffix.map(x => "." + x).getOrElse("")}""")
  }

  def addQuery(p: String): UriBuilder = {
    val q = if (p.startsWith("?")) p.substring(1) else p
    withQuery(query.map(x => s"$x&$q").getOrElse(q))
  }

  def addQuery(q: Map[String, String]): UriBuilder = addQuery(q.toVector)

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

  private def _make_url_query_params(q: Seq[(String, String)]) =
    StringUtils.urlQueryString(q)
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

  def apply(url: URL): UriBuilder = {
    val proto = url.getProtocol
    val auth = url.getAuthority
    val path = url.getPath
    val q = Option(url.getQuery)
    val frag = Option(url.getRef)
    UriBuilder(Some(proto), Some(auth), path, q, frag)
  }

  def apply(uri: String): UriBuilder = apply(new URI(uri))
 
  def byPath(path: String): UriBuilder = UriBuilder(
    None,
    None,
    path,
    None,
    None
  )

  def byPathQuery(path: String, query: Seq[(String, String)]): UriBuilder =
    UriBuilder(
      None,
      None,
      path,
      None,
      None
    )

  def buildByPathQuery(path: String, query: Seq[(String, String)]): URI =
    byPathQuery(path, query).build
}
