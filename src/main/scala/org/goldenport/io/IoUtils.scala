package org.goldenport.io

import java.io.InputStream
import java.nio.charset.Charset
import java.net.{URL, URI}
import scalax.io._

/*
 * @since   Oct.  9, 2017
 * @version Sep. 18, 2018
 * @author  ASAMI, Tomoharu
 */
object IoUtils {
  def toText(url: URL): String = Resource.fromURL(url).string
  def toText(in: InputStream): String = toText(in, Codec.UTF8)
  def toText(in: InputStream, encoding: String): String =
    toText(in, Codec(encoding))
  def toText(in: InputStream, charset: Option[Charset]): String =
    Resource.fromInputStream(in).string(Codec.UTF8)
  def toText(in: InputStream, charset: Charset): String =
    Resource.fromInputStream(in).string(Codec(charset))
  def toText(in: InputStream, codec: Codec): String =
    Resource.fromInputStream(in).string(codec)
}

