package org.goldenport.monitor

import java.io.File
import org.goldenport.recorder._

/*
 * Derived from IRMonitor.java and AbstractRMonitor.java since Feb. 5, 2006.
 * Derived from GMonitor and DefaultMonitor.
 * 
 * @since   Oct. 28, 2008
 *  version Jan. 26, 2009
 *  version May. 17, 2020
 *  version Jun.  1, 2020
 * @version Jan. 24, 2021
 * @author  ASAMI, Tomoharu
 */
trait Monitor {
  def isPlatformWindows: Boolean = System.getProperty("os.name").indexOf("Windows") != -1
  def userHome: File = new File(System.getProperty("user.home"))
  def userDir: File = new File(System.getProperty("user.dir"))
  lazy val recorder: Recorder = new StandardRecorder()
}

object Monitor {
  val default = new DefaultMonitor()

  class DefaultMonitor() extends Monitor {
  }

  def create(args: Array[String]): Monitor = default
}
