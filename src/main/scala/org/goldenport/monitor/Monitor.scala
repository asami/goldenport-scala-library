package org.goldenport.monitor

/*
 * Derived from IRMonitor.java and AbstractRMonitor.java since Feb. 5, 2006.
 * Derived from GMonitor and DefaultMonitor.
 * 
 * @since   Oct. 28, 2008
 *  version Jan. 26, 2009
 *  version May. 17, 2020
 * @version Jun.  1, 2020
 * @author  ASAMI, Tomoharu
 */
trait Monitor {
  def isPlatformWindows: Boolean = System.getProperty("os.name").indexOf("Windows") != -1
}

object Monitor {
  val default = new DefaultMonitor()

  class DefaultMonitor() extends Monitor {
  }

  def create(args: Array[String]): Monitor = default
}
