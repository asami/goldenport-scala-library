package org.goldenport.io

import java.io._
import java.nio.charset.Charset
import java.net.{URL, URI}
import java.util.Base64
import scalax.io._
import com.asamioffice.goldenport.io.{UIO, UURL}
import org.goldenport.bag.Bag

/*
 * @since   Oct.  9, 2017
 *  version Sep. 18, 2018
 *  version Oct.  8, 2018
 *  version May. 19, 2019
 *  version Jun. 24, 2019
 *  version Dec.  7, 2019
 *  version Mar. 18, 2020
 *  version May.  4, 2020
 * @version Jun.  2, 2020
 * @author  ASAMI, Tomoharu
 */
object IoUtils {
  def toText(in: String): String = toText(UURL.getURLFromFileOrURLName(in))
  def toText(in: String, encoding: String): String = toText(UURL.getURLFromFileOrURLName(in), encoding)
  def toText(in: String, charset: Charset): String = toText(UURL.getURLFromFileOrURLName(in), charset)
  def toText(in: String, codec: Codec): String = toText(UURL.getURLFromFileOrURLName(in), codec)

  def toText(url: URL): String = Resource.fromURL(url).string
  def toText(url: URL, encoding: String): String = Resource.fromURL(url).string(Codec(encoding))
  def toText(url: URL, charset: Charset): String = Resource.fromURL(url).string(Codec(charset))
  def toText(url: URL, codec: Codec): String = Resource.fromURL(url).string(codec)

  def toText(uri: URI): String = toText(uri.toString)
  def toText(uri: URI, encoding: String): String = toText(uri.toString, encoding)
  def toText(uri: URI, charset: Charset): String = toText(uri.toString, charset)
  def toText(uri: URI, codec: Codec): String = toText(uri.toString, codec)

  def toText(file: File): String = Resource.fromFile(file).string
  def toText(file: File, encoding: String): String = Resource.fromFile(file).string(Codec(encoding))
  def toText(file: File, charset: Charset): String = Resource.fromFile(file).string(Codec(charset))
  def toText(file: File, codec: Codec): String = Resource.fromFile(file).string(codec)

  def toText(in: InputStream): String = toText(in, Codec.UTF8)
  def toText(in: InputStream, encoding: String): String =
    toText(in, Codec(encoding))
  def toText(in: InputStream, charset: Option[Charset]): String =
    Resource.fromInputStream(in).string(charset.map(Codec(_)) getOrElse Codec.UTF8)
  def toText(in: InputStream, charset: Charset): String =
    Resource.fromInputStream(in).string(Codec(charset))
  def toText(in: InputStream, codec: Codec): String =
    Resource.fromInputStream(in).string(codec)

  def toText(in: InputSource): String = toText(in.openInputStream, Codec.UTF8)
  def toText(in: InputSource, encoding: String): String = toText(in.openInputStream, encoding)
  def toText(in: InputSource, charset: Option[Charset]): String = toText(in.openInputStream, charset)
  def toText(in: InputSource, charset: Charset): String = toText(in.openInputStream, charset)
  def toText(in: InputSource, codec: Codec): String = toText(in.openInputStream, codec)

  def toText(in: ResourceHandle): String = toText(in.openInputStream, Codec.UTF8)
  def toText(in: ResourceHandle, encoding: String): String = toText(in.openInputStream, encoding)
  def toText(in: ResourceHandle, charset: Option[Charset]): String = toText(in.openInputStream, charset)
  def toText(in: ResourceHandle, charset: Charset): String = toText(in.openInputStream, charset)
  def toText(in: ResourceHandle, codec: Codec): String = toText(in.openInputStream, codec)

  def copyClose(in: InputStream, out: OutputStream): Unit =
    for {
      i <- resource.managed(in)
      o <- resource.managed(out)
    } {
      UIO.stream2stream(in, out)
    }

  def toInputStream(p: String): InputStream = new StringInputStream(p)
  def toInputStream(p: String, charset: Charset): InputStream = new StringInputStream(p, charset)

  def save(file: File, p: String, charset: Charset) {
    ensureParentDirectory(file)
    val in = toInputStream(p, charset)
    val out = new FileOutputStream(file)
    copyClose(in, out)
  }

  def save(file: File, url: URL) {
    ensureParentDirectory(file)
    val in = url.openStream
    val out = new FileOutputStream(file)
    copyClose(in, out)
  }

  def save(file: File, bag: Bag) {
    ensureParentDirectory(file)
    for {
      out <- resource.managed(new FileOutputStream(file))
    } {
      bag.copyTo(out)
    }
  }

  def ensureParentDirectory(p: File) {
    Option(p.getParentFile).map(ensureDirectory)
  }

  def ensureDirectory(p: File) {
    p.mkdirs
  }

  def descendants(p: File): Vector[File] = {
    case class Z(
      files: Vector[File] = Vector.empty,
      indirs: Vector[File] = Vector.empty
    ) {
      def r = files ++ indirs

      def +(rhs: File) = {
        if (rhs.isFile)
          copy(files = files :+ rhs)
        else
          copy(indirs = indirs ++ descendants(rhs))
      }
    }
    p.listFiles.toList./:(Z())(_+_).r
  }

  def openInputStream(url: URL): InputStream = openInputStream(url, None, None)

  def openInputStream(url: URL, user: String, password: String): InputStream =
    _open_inputstream(url, s"$user:$password")

  def openInputStream(url: URL, user: Option[String], password: Option[String]): InputStream =
    Option(url.getUserInfo).map(x => _open_inputstream(url, x)).getOrElse(
      user.map(x => _open_inputstream(url, x, password)).getOrElse(url.openStream)
    )

  private def _open_inputstream(url: URL, user: String, password: Option[String]): InputStream = {
    val s = password.map(x => s"$user:$x").getOrElse(user)
    _open_inputstream(url, s)
  }

  private def _open_inputstream(url: URL, userpassword: String): InputStream = {
    val data = Base64.getEncoder().encodeToString(userpassword.getBytes())
    val conn = url.openConnection()
    conn.setRequestProperty("Authorization", s"Basic $data")
    conn.getInputStream()
  }
}
