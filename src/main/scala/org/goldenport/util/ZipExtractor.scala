package org.goldenport.util

import scala.util.Try
import java.io.{File, BufferedInputStream, FileOutputStream}
import java.net.URL
import java.util.zip.{ZipInputStream, ZipEntry}

/*
 * @since   Nov.  6, 2017
 * @version Nov.  6, 2017
 * @author  ASAMI, Tomoharu
 */
case class ZipExtractor(
  isFileMaxSizeCheck: Boolean = true,
  fileMaxSize: Int = 1024 * 1024 * 16, // 16M
  bufferSize: Int = 8192
) {
  def apply(dest: File, url: URL) {
    for (in <- resource.managed(url.openStream())) {
      val zis = new ZipInputStream(new BufferedInputStream(in))
      var entry = zis.getNextEntry()
      while (entry != null) {
        _output_file_or_directory(dest, zis, entry)
        entry = zis.getNextEntry()
      }
    }
  }

  private def _output_file_or_directory(homedir: File, zis: ZipInputStream, entry: ZipEntry) {
    if (entry.isDirectory)
      _output_directory(homedir, zis, entry)
    else
      _output_file(homedir, zis, entry)
  }

  private def _output_file(homedir: File, zis: ZipInputStream, entry: ZipEntry) {
    val maxchunk = fileMaxSize / bufferSize
    val buf = new Array[Byte](bufferSize)
    val entryname = entry.getName
    val file = new File(homedir, entryname)
    for (out <- resource.managed(new FileOutputStream(file))) {
      val iter = Iterator.continually(zis.read(buf, 0, bufferSize)).takeWhile(_ != -1)
      for ((c, i) <- iter.zipWithIndex) {
        if (isFileMaxSizeCheck && i > maxchunk)
          throw new IllegalStateException(s"File in zip is too large: $entryname")
        out.write(buf, 0, c)
      }
    }
  }

  private def _output_directory(homedir: File, zis: ZipInputStream, entry: ZipEntry) {
    val entryname = entry.getName
    val dir = new File(homedir, entryname)
    dir.mkdirs
  }
}

object ZipExtractor {
  lazy val noFileMaxSizeCheck = ZipExtractor(false)
}
