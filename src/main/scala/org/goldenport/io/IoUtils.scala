package org.goldenport.io

import java.io._
import java.nio.charset.Charset
import java.net.{URL, URI}
import scalax.io._
import com.asamioffice.goldenport.io.{UIO, UURL}

/*
 * @since   Oct.  9, 2017
 *  version Sep. 18, 2018
 *  version Oct.  8, 2018
 * @version May. 19, 2019
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
    Resource.fromInputStream(in).string(Codec.UTF8)
  def toText(in: InputStream, charset: Charset): String =
    Resource.fromInputStream(in).string(Codec(charset))
  def toText(in: InputStream, codec: Codec): String =
    Resource.fromInputStream(in).string(codec)

  def copyClose(in: InputStream, out: OutputStream): Unit =
    for {
      i <- resource.managed(in)
      o <- resource.managed(out)
    } {
      UIO.stream2stream(in, out)
    }
}
