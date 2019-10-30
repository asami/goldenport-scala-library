package org.goldenport.log

import java.net.URL
import org.slf4j._

/*
 * @since   Oct. 27, 2019
 * @version Oct. 27, 2019
 * @author  ASAMI, Tomoharu
 */
case class LogConfig(
  level: Option[LogLevel],
  // format, output
  confFile: Option[URL]
) {
  def withLogLevel(p: LogLevel) = copy(level = Some(p))
  def withConfFile(p: URL) = copy(confFile = Some(p))
}

object LogConfig {
  val empty = LogConfig(None, None)
}

