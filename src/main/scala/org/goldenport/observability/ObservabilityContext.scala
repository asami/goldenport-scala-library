package org.goldenport.observability

import org.goldenport.log._

/*
 * @since   Apr. 28, 2025
 * @version Apr. 28, 2025
 * @author  ASAMI, Tomoharu
 */
case class ObservabilityContext(
) {
  def log(mark: Option[LogMark], level: LogLevel, msg: String): Unit =
    mark.map(log(_, level, msg)).getOrElse(log(level, msg))

  def log(mark: LogMark, level: LogLevel, msg: String): Unit = {
    LogContext.log(mark, level, msg)
  }

  def log(level: LogLevel, msg: String): Unit = {
    LogContext.log(level, msg)
  }

  def trace(): Unit = {
  }

  def metrics(): Unit = {
  }
}

object ObservabilityContext {
  val default = ObservabilityContext()
}
