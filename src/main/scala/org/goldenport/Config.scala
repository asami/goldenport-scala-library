package org.goldenport

import java.io.File

/*
 * @since   Oct. 21, 2014
 * @version Aug. 29, 2017
 * @author  ASAMI, Tomoharu
 */
object Config {
  private var _temp_dir: Option[File] = None

  def getTempDir: Option[File] = _temp_dir
  def setTempDir(dir: File) { _temp_dir = Some(dir) }
}
