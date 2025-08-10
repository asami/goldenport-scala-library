package org.goldenport.io

import scala.util.control.NonFatal
import java.net.{URI, URL}
import java.net.URLEncoder
import java.io.File
import java.nio.file.{Paths, Path, InvalidPathException}
import java.nio.charset.StandardCharsets
import com.asamioffice.goldenport.io.UURL
import org.goldenport.values.Urn
import org.goldenport.cli.Environment
import org.goldenport.util

/*
 * @since   Oct.  6, 2017
 *  version Apr. 26, 2019
 *  version Jan. 26, 2020
 *  version Nov. 22, 2023
 *  version May. 29, 2024
 *  version Jul. 18, 2025
 * @version Aug. 10, 2025
 * @author  ASAMI, Tomoharu
 */
object UriUtils {
  def addPath(uri: URI, path: String) = UriBuilder(uri).addPath(path).build
  def sibling(uri: URI): URI = UriBuilder.byPath("..").addPath(uri).build

  def getFile(uri: URI): Option[File] =
    Option(uri.getScheme) match {
      case Some(s) => s.toLowerCase match {
        case "file" => Some(new File(uri.toURL.getFile))
        case _ => None
      }
      case None => Some(new File(uri.getPath))
    }

  def getFile(base: File, uri: URI): Option[File] =
    Option(uri.getScheme) match {
      case Some(s) => s.toLowerCase match {
        case "file" => Some(new File(uri.toURL.getFile))
        case _ => None
      }
      case None => Some(new File(base, uri.getPath))
    }

  def getPath(uri: URI): Option[Path] = getFile(uri).map(_.toPath)

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
    val s = p.toVector.foldLeft(Z())(_+_).r
    new java.net.URI(s)
  }

  def toContainer(uri: URI): URI = UriBuilder(uri).container.build

  def showTerse(workdir: File, uri: URI): String =
    util.UriUtils.showTerse(workdir, uri)

  def showTerse(env: Environment, uri: URI): String =
    util.UriUtils.showTerse(env, uri)

  def getRelative(base: File, uri: URI): Option[URI] =
    util.UriUtils.getRelative(base, uri)

  def getRelative(base: URI, uri: URI): Option[URI] =
    util.UriUtils.getRelative(base, uri)

  def printPathQueryFragment(p: URI): String =
    util.UriUtils.printPathQueryFragment(p)
}
