package org.goldenport.config

import com.typesafe.config.{Config => Hocon, ConfigFactory => HoconFactory}
import org.slf4j._
import org.goldenport.log._
import org.goldenport.util.HoconUtils.RichConfig

/*
 * @since   Sep. 16, 2018
 * @version Oct.  6, 2018
 * @author  ASAMI, Tomoharu
 */
trait Config {
  // typesafe config
  // locale
  // encoding
  // timezone
  // datetime format
  // message resource
  // logger

  protected def properties: RichConfig // Typesafe Config (Human-Optimized Config Object Notation)
  protected def logLevel: LogLevel

  // private def _set_root_logger_level {
  //   LoggerFactory.getLogger(Logger.ROOT_LOGGER_NAME) match {
  //     case m: ch.qos.logback.classic.Logger =>
  //       logLevel.map(m.setLevel(_))
  //   }
  // }

  private def _set_root_logger_level(p: ch.qos.logback.classic.LoggerContext) {
    val x = LogbackUtils.toLogbackLevel(logLevel)
    val root = p.getLogger(Logger.ROOT_LOGGER_NAME)
    root.setLevel(x)
  }

  private lazy val _logger_factory: ILoggerFactory = {
    LoggerFactory.getILoggerFactory() match {
      case m: ch.qos.logback.classic.LoggerContext =>
        _set_root_logger_level(m)
        m
      case m => m
    }
  }
  lazy val logger = _logger_setup(_logger_factory.getLogger(""))
  private var _logger_by_name: Map[String, Logger] = Map.empty

  def logger(mark: LogMark): Logger = _logger_by_name.
    get(mark.name).getOrElse {
      val r = _logger_setup(_logger_factory.getLogger(mark.name))
      _logger_by_name += (mark.name -> r)
      r
    }

  private def _logger_setup(p: Logger): Logger = {
    p
  }
}

object Config {
  case class BasicConfig(
    logLevel: LogLevel,
    properties: RichConfig
  ) extends Config {
  }

  def loadHocon(): RichConfig = {
    RichConfig(HoconFactory.load()) // TODO home, current directories
  }
}
