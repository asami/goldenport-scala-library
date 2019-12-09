package org.goldenport.io

import java.io.{InputStream, OutputStream, File}
import java.net.URL
import java.nio.charset.Charset
import scalax.io.Codec
import org.goldenport.Platform
import org.goldenport.bag._

/*
 * See ResourceHandle
 * 
 * @since   Oct.  5, 2018
 *  version Oct.  8, 2018
 * @version Dec.  7, 2019
 * @author  ASAMI, Tomoharu
 */
trait InputSource {
  def openInputStream: InputStream = asBag.openInputStream
  def writeClose(out: OutputStream): Unit = IoUtils.copyClose(openInputStream, out)
  def asBag: ChunkBag = BufferFileBag.fromInputStreamAndClose(openInputStream)
  def asText: String = IoUtils.toText(this)
  def asText(encoding: String): String = IoUtils.toText(this, encoding)
  def asText(charset: Option[Charset]): String = IoUtils.toText(this, charset)
  def asText(charset: Charset): String = IoUtils.toText(this, charset)
  def asText(codec: Codec): String = IoUtils.toText(this, codec)
}

case class StringInputSource(
  string: String,
  charset: Charset = Platform.charset.UTF8
) extends InputSource {
  override lazy val asBag = BufferBag.create(string, charset)
}

case class FileInputSource(file: File) extends InputSource {
  override def asBag = FileBag.create(file)
}

case class UrlInputSource(url: URL) extends InputSource {
  override def openInputStream = url.openStream
}
