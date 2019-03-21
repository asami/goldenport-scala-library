package org.goldenport

import java.io.File
import java.nio.charset.Charset
import scalax.file.Path
import scalax.file.defaultfs.DefaultPath
import scalax.io.Codec
import com.asamioffice.goldenport.io.UFile

/*
 * @since   Oct. 21, 2014
 *  version Dec. 19, 2014
 *  version Sep. 24, 2015
 *  version Jul. 29, 2017
 *  version Aug. 29, 2017
 * @version Oct. 15, 2018
 * @author  ASAMI, Tomoharu
 */
object Platform {
  private def _temp_path_base: Option[String] = Config.getTempDir.map(_ + "/")

  def createTempFile(prefix: String = "goldenport", suffix: String = ""): File = {
    Config.getTempDir match {
      case Some(s) => File.createTempFile(prefix, suffix, s)
      case None => File.createTempFile(prefix, suffix)
    }
  }

  def createTempDirectory(prefix: String = "goldenport", suffix: String = ".d"): File = {
    val dir = createTempFile(prefix, suffix)
    dir.delete()
    dir.mkdir()
    dir.deleteOnExit()
    dir
  }

  def createTempPath(prefix: String = "goldenport", suffix: String = ""): DefaultPath = {
    Config.getTempDir match {
      case Some(s) => Path.createTempFile(prefix, suffix, s.getAbsolutePath())
      case None => Path.createTempFile(prefix, suffix)
    }
  }

  def deleteTempFiles(file: File) {
    deleteTempFiles(file.toString)
  }

  def deleteTempFiles(path: String) {
    _temp_path_base match {
      case Some("/") => throw new IllegalStateException(s"Invalid temporary base directory: ${_temp_path_base}")
      case Some(s) =>
        if (path.startsWith(s) && path.length > s.length)
          // UFile.deleteWholeFiles(path)
          println(s"Platform#deleteWholeFiles: $path") // for debug
        else
          throw new IllegalArgumentException(s"Invalid temporary file: $path")
      case None => Unit
    }
  }

  object codec {
    val UTF8 = Codec.UTF8
    val WINDOWS31J = Codec(Charset forName "Windows-31J")
  }

  object charset {
    val UTF8 = Codec.UTF8.charSet
    val WINDOWS31J = Charset forName "Windows-31J"
  }
}
