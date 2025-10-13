package org.goldenport.observability

import org.goldenport.log._

/*
 * @since   Apr. 28, 2025
 *  version Jul. 27, 2025
 * @version Oct. 11, 2025
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

  object log {
    def error(msg: String) = log(LogLevel.Error, msg)
    def warn(msg: String) = log(LogLevel.Warn, msg)
    def info(msg: String) = log(LogLevel.Info, msg)
    def debug(msg: String) = log(LogLevel.Debug, msg)
    def trace(msg: String) = log(LogLevel.Trace, msg)
  }

  def trace(): Unit = {
  }

  def metrics(): Unit = {
  }
}

object ObservabilityContext {
  val default = ObservabilityContext()
}
