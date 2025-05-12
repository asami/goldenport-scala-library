package org.goldenport.log

import java.net.URL
import org.slf4j._
import com.typesafe.config.{Config => Hocon}
import com.asamioffice.goldenport.io.UURL
import org.goldenport.RAISE
import org.goldenport.context.Consequence
import org.goldenport.cli.ConfigurationParseState
import org.goldenport.util.StringUtils
import org.goldenport.hocon.RichConfig.Implicits._
import ConfigurationParseState.StateFunctions._

/*
 * @since   Oct. 27, 2019
 * @version May. 11, 2025
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
  val PROP_LOG_STRATEGY = "log.strategy"
  val PROP_LOG_LEVEL = "log.level"

  val empty = LogConfig(None, None)

  def parse(p: ConfigurationParseState): ConfigurationParseState.Result[LogConfig] = {
    val r = for {
      level <- cStringOption(PROP_LOG_LEVEL).map(_.map(_level))
      url <- cStringOption(PROP_LOG_STRATEGY).map(_.map(_url))
    } yield LogConfig(level, url)
    val (s, logconfig) = r.run(p).take
    ConfigurationParseState.Result(s, logconfig)
  }

  def create(p: Hocon): LogConfig = {
    val level = p.getStringOption(PROP_LOG_LEVEL).map(_level)
    val url = p.getStringOption(PROP_LOG_STRATEGY).map(_url)
    LogConfig(level, url)
  }

  private def _level(p: String) = {
    LogLevel.get(p) getOrElse RAISE.invalidArgumentFault(s"log.level: $p")
  }

  private def _url(p: String) = {
    if (StringUtils.getSuffix(p).isDefined) {
      UURL.getURLFromFileOrURLName(p)
    } else {
      val name = s"logback-${p}.xml"
      Option(getClass.getClassLoader.getResource(name)) getOrElse RAISE.invalidArgumentFault(s"${PROP_LOG_STRATEGY}: $p")
    }
  }
}
