package org.goldenport.io

import java.io.{InputStream, OutputStream, File}
import java.net.URL
import java.nio.charset.Charset
import java.nio.file.Path
import scalax.io.Codec
import com.asamioffice.goldenport.io.UURL
import org.goldenport.Platform
import org.goldenport.context._
import org.goldenport.bag._

/*
 * See ResourceHandle
 * 
 * @since   Oct.  5, 2018
 *  version Oct.  8, 2018
 *  version Dec.  7, 2019
 *  version Jul. 31, 2023
 * @version Mar. 14, 2025
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
object InputSource {
  def apply(string: String): InputSource = StringInputSource(string)
  def apply(string: String, charset: Charset): InputSource = StringInputSource(string, charset)
  def apply(file: File): InputSource = FileInputSource(file)
  def apply(path: Path): InputSource = PathInputSource(path)
  def apply(url: URL): InputSource = UrlInputSource(url)
  def apply(bag: Bag): InputSource = BagInputSource(bag)

  def create(s: Any): InputSource = s match {
    case m: File => apply(m)
    case m: URL => apply(m)
    case m: String => _create(m)
    case m => ValueDomainValueFault(s"Not InputSource: $s").RAISE
  }

  private def _create(s: String): InputSource = UrlInputSource(UURL.getURLFromFileOrURLName(s))

  def file(p: String): InputSource = FileInputSource(new File(p))
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

case class PathInputSource(path: Path) extends InputSource {
  override def asBag = FileBag.create(path.toFile)
}

case class UrlInputSource(url: URL) extends InputSource {
  override def openInputStream = url.openStream
}

case class BagInputSource(bag: Bag) extends InputSource {
  override def asBag = bag.toChunkBag
}
