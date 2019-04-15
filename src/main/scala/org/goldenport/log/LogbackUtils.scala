package org.goldenport.log

import ch.qos.logback.classic._

/*
 * @since   Oct.  6, 2018
 *  version Oct.  6, 2018
 * @version Feb. 24, 2019
 * @author  ASAMI, Tomoharu
 */
object LogbackUtils {
  def toLogbackLevel(p: LogLevel) = p match {
    case LogLevel.Error => Level.ERROR
    case LogLevel.Warn => Level.WARN
    case LogLevel.Info => Level.INFO
    case LogLevel.Debug => Level.DEBUG
    case LogLevel.Trace => Level.TRACE
  }

  def toLogLevel(p: Level) = p match {
    case Level.ERROR => LogLevel.Error
    case Level.WARN => LogLevel.Warn
    case Level.INFO => LogLevel.Info
    case Level.DEBUG => LogLevel.Debug
    case Level.TRACE => LogLevel.Trace
  }
}
