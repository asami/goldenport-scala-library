package org.goldenport.io

import scala.util.Try
import java.io.File
import java.io.{InputStream, OutputStream}
import java.io.{FileInputStream, FileOutputStream}
import java.nio.charset.Charset
import java.nio.file.Path
import java.nio.file.Files
import java.net.URL
import org.goldenport.RAISE

/*
 * @since   Mar. 14, 2025
 * @version Mar. 14, 2025
 * @author  ASAMI, Tomoharu
 */
trait OutputSink {
  def getOpenInputStream(): Option[InputStream]
  def openOutputStream(): OutputStream
}

object OutputSink {
  def apply(file: File): OutputSink = FileOutputSink(file)
  def apply(path: Path): OutputSink = PathOutputSink(path)
  def make(url: URL): OutputSink = {
    val file = UrlUtils.getFile(url) getOrElse RAISE.unsupportedOperationFault("Not file")
    FileOutputSink(file)
  }
}

case class FileOutputSink(file: File) extends OutputSink {
  def openInputStream(): InputStream = new FileInputStream(file)
  def getOpenInputStream(): Option[InputStream] = Try(openInputStream()).toOption
  def openOutputStream(): OutputStream = new FileOutputStream(file)
}

case class PathOutputSink(path: Path) extends OutputSink {
  def openInputStream(): InputStream = Files.newInputStream(path)
  def getOpenInputStream(): Option[InputStream] = Try(openInputStream()).toOption
  def openOutputStream(): OutputStream = Files.newOutputStream(path)
}
