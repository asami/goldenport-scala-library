package org.goldenport.recorder.driver

/*
 * @since   Jan. 26, 2021
 * @version Jan. 26, 2021
 * @author  ASAMI, Tomoharu
 */
class ConsoleLogDriver() extends LogDriver {
}

object ConsoleLogDriver {
  val default = new ConsoleLogDriver()
}
