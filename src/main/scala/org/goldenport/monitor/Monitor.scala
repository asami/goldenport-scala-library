package org.goldenport.monitor

import java.io.File

/*
 * Derived from IRMonitor.java and AbstractRMonitor.java since Feb. 5, 2006.
 * Derived from GMonitor and DefaultMonitor.
 * 
 * @since   Oct. 28, 2008
 *  version Jan. 26, 2009
 *  version May. 17, 2020
 *  version Jun.  1, 2020
 * @version Jan. 11, 2021
 * @author  ASAMI, Tomoharu
 */
trait Monitor {
  // TODO log, messager, notifier
  def isPlatformWindows: Boolean = System.getProperty("os.name").indexOf("Windows") != -1
  def userHome: File = new File(System.getProperty("user.home"))
  def userDir: File = new File(System.getProperty("user.dir"))
}

object Monitor {
  val default = new DefaultMonitor()

  class DefaultMonitor() extends Monitor {
  }

  def create(args: Array[String]): Monitor = default
}
