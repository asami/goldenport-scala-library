package org.goldenport.config

import org.goldenport.log.LogMark

/*
 * @since   Sep. 16, 2018
 * @version Sep. 17, 2018
 * @author  ASAMI, Tomoharu
 */
trait ConfigHelper {
  def config: Config

  private def _logger = config.logger
  private def _logger(p: LogMark) = config.logger(p)

  object log {
    def error(msg: => String): Unit = if (_logger.isErrorEnabled) _logger.error(msg)
    def error(msg: => String, e: Throwable): Unit = if (_logger.isErrorEnabled) _logger.error(msg, e)
    def warn(msg: => String): Unit = if (_logger.isWarnEnabled) _logger.warn(msg)
    def warn(msg: => String, e: Throwable): Unit = if (_logger.isWarnEnabled) _logger.warn(msg, e)
    def info(msg: => String): Unit = if (_logger.isInfoEnabled) _logger.info(msg)
    def debug(msg: => String): Unit = if (_logger.isDebugEnabled) _logger.debug(msg)
    def trace(msg: => String): Unit = if (_logger.isTraceEnabled) _logger.trace(msg)

    def error(mark: LogMark, msg: => String): Unit = {
      val logger = _logger(mark)
      if (logger.isErrorEnabled)
        logger.error(mark.marker, s"${mark.name}:$msg")
    }
    def error(mark: LogMark, msg: => String, e: Throwable): Unit = {
      val logger = _logger(mark)
      if (logger.isErrorEnabled)
        logger.error(mark.marker, s"${mark.name}:$msg", e)
    }
    def warn(mark: LogMark, msg: => String): Unit = {
      val logger = _logger(mark)
      if (logger.isWarnEnabled)
        logger.warn(mark.marker, s"${mark.name}:$msg")
    }
    def warn(mark: LogMark, msg: => String, e: Throwable): Unit = {
      val logger = _logger(mark)
      if (logger.isWarnEnabled)
        logger.warn(mark.marker, s"${mark.name}:$msg", e)
    }
    def info(mark: LogMark, msg: => String): Unit = {
      val logger = _logger(mark)
      if (logger.isInfoEnabled)
        logger.info(mark.marker, s"${mark.name}:$msg")
    }
    def debug(mark: LogMark, msg: => String): Unit = {
      val logger = _logger(mark)
      if (logger.isDebugEnabled)
        logger.debug(mark.marker, s"${mark.name}:$msg")
    }
    def trace(mark: LogMark, msg: => String): Unit = {
      val logger = _logger(mark)
      if (logger.isTraceEnabled)
        logger.trace(mark.marker, s"${mark.name}:$msg")
    }

    def error(location: LogMark.Location, action: LogMark.Action, label: String, msg: => String): Unit =
      error(LogMark(location, action, label), msg)
    def error(location: LogMark.Location, action: LogMark.Action, label: String, msg: => String, e: Throwable): Unit =
      error(LogMark(location, action, label), msg, e)
    def warn(location: LogMark.Location, action: LogMark.Action, label: String, msg: => String): Unit =
      warn(LogMark(location, action, label), msg)
    def warn(location: LogMark.Location, action: LogMark.Action, label: String, msg: => String, e: Throwable): Unit =
      warn(LogMark(location, action, label), msg, e)
    def info(location: LogMark.Location, action: LogMark.Action, label: String, msg: => String): Unit =
      info(LogMark(location, action, label), msg)
    def debug(location: LogMark.Location, action: LogMark.Action, label: String, msg: => String): Unit =
      debug(LogMark(location, action, label), msg)
    def trace(location: LogMark.Location, action: LogMark.Action, label: String, msg: => String): Unit =
      trace(LogMark(location, action, label), msg)
  }
}

