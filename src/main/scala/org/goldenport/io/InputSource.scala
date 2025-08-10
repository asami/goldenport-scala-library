package org.goldenport.io

import java.io.{InputStream, OutputStream, File}
import java.io.Reader
import java.io.FileInputStream
import java.io.BufferedInputStream
import java.io.FileReader
import java.io.StringReader
import java.net.URL
import java.net.URI
import java.nio.charset.Charset
import java.nio.file.Files
import java.nio.file.Path
import scalax.io.Codec
import com.asamioffice.goldenport.io.UURL
import org.goldenport.Platform
import org.goldenport.context._
import org.goldenport.bag._
import org.goldenport.util.StringUtils

/*
 * See ResourceHandle
 * 
 * @since   Oct.  5, 2018
 *  version Oct.  8, 2018
 *  version Dec.  7, 2019
 *  version Jul. 31, 2023
 *  version Mar. 20, 2025
 *  version Jun. 14, 2025
 *  version Jul. 24, 2025
 * @version Aug.  6, 2025
 * @author  ASAMI, Tomoharu
 */
trait InputSource {
  def openInputStream: InputStream = asBag.openInputStream
  def openReader: Reader = asBag.openReader(Platform.charset.UTF8)
  def openReader(enc: Charset): Reader = asBag.openReader(enc)
  def writeClose(out: OutputStream): Unit = IoUtils.copyClose(openInputStream, out)
  def asBag: ChunkBag = BufferFileBag.fromInputStreamAndClose(openInputStream)
  def asText: String = IoUtils.toText(this)
  def asText(encoding: String): String = IoUtils.toText(this, encoding)
  def asText(charset: Option[Charset]): String = IoUtils.toText(this, charset)
  def asText(charset: Charset): String = IoUtils.toText(this, charset)
  def asText(codec: Codec): String = IoUtils.toText(this, codec)
  def getSuffix: Option[String]
}
object InputSource {
  def apply(string: String): InputSource = StringInputSource(string)
  def apply(string: String, charset: Charset): InputSource = StringInputSource(string, charset)
  def apply(string: String, uri: URI): InputSource = StringInputSource(string, uri)
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

  def string(p: String): InputSource = StringInputSource(p)

  def file(p: String): InputSource = FileInputSource(new File(p))
}

case class StringInputSource(
  string: String,
  charset: Charset = Platform.charset.UTF8,
  uri: Option[URI] = None
) extends InputSource {
  override lazy val asBag = BufferBag.create(string, charset)

  override def openReader = openReader(charset)

  override def openReader(p: Charset): Reader = new StringReader(string)

  def getSuffix = uri.flatMap(x => StringUtils.getSuffix(x.toString))
}
object StringInputSource {
  def apply(s: String, uri: URI): StringInputSource =
    StringInputSource(s, Platform.charset.UTF8, Some(uri))
}

case class FileInputSource(file: File) extends InputSource {
  override def asBag = FileBag.create(file)

  override def openInputStream: InputStream = new BufferedInputStream(Files.newInputStream(file.toPath))
  override def openReader: Reader = Files.newBufferedReader(file.toPath, Platform.charset.UTF8)
  override def openReader(enc: Charset): Reader = Files.newBufferedReader(file.toPath, enc)

  def getSuffix = StringUtils.getSuffix(file.getName)
}

case class PathInputSource(path: Path) extends InputSource {
  override def asBag = FileBag.create(path.toFile)

  def getSuffix = StringUtils.getSuffix(path.toString)

}

case class UrlInputSource(url: URL) extends InputSource {
  override def openInputStream = url.openStream

  def getSuffix = StringUtils.getSuffix(url.toString)
}

case class BagInputSource(bag: Bag) extends InputSource {
  override def asBag = bag.toChunkBag

  def getSuffix = bag.filenameSuffix
}
