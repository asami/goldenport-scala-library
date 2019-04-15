package org.goldenport.io

import java.io.{InputStream, OutputStream, File}
import java.net.URL
import java.nio.charset.Charset
import org.goldenport.Platform
import org.goldenport.bag._

/*
 * @since   Oct.  5, 2018
 * @version Oct.  8, 2018
 * @author  ASAMI, Tomoharu
 */
trait InputSource {
  def asBag: ChunkBag = BufferFileBag.fromInputStreamAndClose(openInputStream)
  def openInputStream: InputStream = asBag.openInputStream
  def writeClose(out: OutputStream): Unit = IoUtils.copyClose(openInputStream, out)
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
