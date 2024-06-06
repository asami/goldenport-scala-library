package org.goldenport.io

import scala.util.control.NonFatal
import java.net.{URI, URL}
import java.net.URLEncoder
import java.io.File
import java.nio.charset.StandardCharsets
import com.asamioffice.goldenport.io.UURL
import org.goldenport.values.Urn

/*
 * @since   Oct.  6, 2017
 *  version Apr. 26, 2019
 *  version Jan. 26, 2020
 *  version Nov. 22, 2023
 * @version May. 29, 2024
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

  private val _url_special_chars = Vector(':', '/', '?', '&', '=')

  def createUriFromUnsafeString(p: String): java.net.URI = {
    case class Z(
      z: Vector[Char] = Vector.empty,
      xs: Vector[Char] = Vector.empty
    ) {
      val r: String = (z ++ xs).mkString

      def +(rhs: Char) =
        if (_url_special_chars.contains(rhs)) {
          val a = z ++ URLEncoder.encode(xs.mkString, StandardCharsets.UTF_8.toString()) :+ rhs
          copy(z = a, xs = Vector.empty)
        } else {
          copy(xs = xs :+ rhs)
        }
    }
    val s = p.toVector./:(Z())(_+_).r
    new java.net.URI(s)
  }
}
