package org.goldenport.log

import java.net.URL
import org.slf4j._
import ch.qos.logback.classic.joran.JoranConfigurator
import ch.qos.logback.core.joran.spi.JoranException
import ch.qos.logback.core.util.StatusPrinter
import org.goldenport.RAISE
import org.goldenport.config._

/*
 * @since   Feb. 16, 2019
 *  version Feb. 24, 2019
 *  version Apr.  8, 2019
 *  version Oct. 27, 2019
 * @version May.  4, 2025
 * @author  ASAMI, Tomoharu
 */
class LogContext(mark: LogMark) {
  def log(level: LogLevel, msg: String) {
  }
}

object LogContext {
  class ThreadContext() {
    private var _logger_by_name: Map[String, Logger] = Map.empty
  }
  private var _logger_by_name: Map[String, Logger] = Map.empty

  private def _logger_factory: ILoggerFactory = LoggerFactory.getILoggerFactory()

  def init(url: Option[URL], level: Option[LogLevel]): ILoggerFactory = {
    _logger_factory match {
      case m: ch.qos.logback.classic.LoggerContext =>
        _init(m, url, level)
        m
      case m => m
    }
  }

  private def _init(
    p: ch.qos.logback.classic.LoggerContext,
    url: Option[URL],
    level: Option[LogLevel]
  ) {
    url.foreach { x =>
      try {
        val configurator = new JoranConfigurator()
        configurator.setContext(p)
        p.reset() // XXX
        configurator.doConfigure(x)
      } catch {
        case m: JoranException => RAISE.notImplementedYetDefect(s"$m")
      }
      StatusPrinter.printInCaseOfErrorsOrWarnings(p)
    }
    level.foreach(_set_root_logger_level(p, _))
  }

  def setRootLevel(level: LogLevel): ILoggerFactory = {
    _logger_factory match {
      case m: ch.qos.logback.classic.LoggerContext =>
        _set_root_logger_level(m, level)
        m
      case m => m
    }
  }

  private def _set_root_logger_level(p: ch.qos.logback.classic.LoggerContext, level: LogLevel) = {
    val x = LogbackUtils.toLogbackLevel(level)
    val root = p.getLogger(Logger.ROOT_LOGGER_NAME)
    root.setLevel(x)
  }

  def takeRootLevel: LogLevel = {
    _logger_factory match {
      case m: ch.qos.logback.classic.LoggerContext =>
        val root = m.getLogger(Logger.ROOT_LOGGER_NAME)
        LogbackUtils.toLogLevel(root.getLevel)
    }
  }

  lazy val defaultLogger: Logger = _logger_setup(_logger_factory.getLogger(""))

  def logger(mark: LogMark): Logger = _logger_factory.getLogger(mark.name)

  def logger_OLD(mark: LogMark): Logger = _logger_by_name.
    get(mark.name).getOrElse {
      val r = _logger_setup(_logger_factory.getLogger(mark.name))
      _logger_by_name += (mark.name -> r)
      r
    }

  private def _logger_setup(p: Logger): Logger = {
    p
  }

  def log(mark: Option[LogMark], level: LogLevel, msg: String): Unit =
    mark.map(log(_, level, msg)).getOrElse(log(level, msg))

  def log(mark: LogMark, level: LogLevel, msg: String): Unit = {
    _log(logger(mark), level, msg)
  }

  def log(level: LogLevel, msg: String): Unit = {
    _log(defaultLogger, level, msg)
  }

  def logDelay(mark: Option[LogMark], level: LogLevel, msg: => String): Unit =
    mark.map(logDelay(_, level, msg)).getOrElse(logDelay(level, msg))

  def logDelay(mark: LogMark, level: LogLevel, msg: => String): Unit = {
    _log_delay(logger(mark), level, msg)
  }

  def logDelay(level: LogLevel, msg: => String): Unit = {
    _log_delay(defaultLogger, level, msg)
  }

  private def _log(logger: Logger, level: LogLevel, msg: String): Unit = {
    level match {
      case LogLevel.Error => logger.error(msg)
      case LogLevel.Warn => logger.warn(msg)
      case LogLevel.Info => logger.info(msg)
      case LogLevel.Debug => logger.debug(msg)
      case LogLevel.Trace => logger.trace(msg)
    }
  }

  private def _log_delay(logger: Logger, level: LogLevel, msg: => String): Unit = {
    level match {
      case LogLevel.Error => if (logger.isErrorEnabled) logger.error(msg)
      case LogLevel.Warn => if (logger.isWarnEnabled) logger.warn(msg)
      case LogLevel.Info => if (logger.isInfoEnabled) logger.info(msg)
      case LogLevel.Debug => if (logger.isDebugEnabled) logger.debug(msg)
      case LogLevel.Trace => if (logger.isTraceEnabled) logger.trace(msg)
    }
  }
}
