package org.goldenport.console

import java.nio.charset.Charset
import org.goldenport.Platform
import org.goldenport.cli.Environment

/*
 * Derived from ConsoleMessager since Aug. 27, 2005
 * 
 * @since   Jan. 10, 2021
 * @version Jan. 10, 2021
 * @author  ASAMI, Tomoharu
 */
class ConsoleManager(
  config: ConsoleManager.Config,
  newline: ConsoleManager.NewlineStrategy,
  hilight: ConsoleManager.HilightStrategy
) {
  private val _device = new SystemConsoleDevice(config)

  def read(): Int = _device.read()

  def print(p: String): Unit = _device.print(_normalize_standard(p))

  def println(p: String): Unit = _device.println(_normalize_standard(p))

  def error(p: String): Unit = _device.error(_normalize_error(p))

  def errorln(p: String): Unit = _device.errorln(_normalize_error(p))

  def warning(p: String): Unit = _device.warning(_normalize_warning(p))

  def warningln(p: String): Unit = _device.warningln(_normalize_warning(p))

  def prompt(p: String): Unit = _device.prompt(_normalize_prompt(p))

  def message(p: Message): Unit = p match {
    case StandardMessage(msg) => println(msg)
    case ErrorMessage(msg) => errorln(msg)
    case WarningMessage(msg) => warningln(msg)
    case Prompt(msg) => prompt(msg)
  }

  def message(p: MessageSequence): Unit = p.messages.map(message)

  private def _normalize_standard(p: String) = hilight.standard(newline(p))

  private def _normalize_error(p: String) = hilight.error(newline(p))

  private def _normalize_warning(p: String) = hilight.warning(newline(p))

  private def _normalize_prompt(p: String) = hilight.prompt(newline(p))
}

object ConsoleManager {
  val default = new ConsoleManager(
    Config.default,
    AsIsNewlineStrategy(),
    NoneHilightStrategy()
  )

  case class Config(
    charset: Charset,
    newline: String
  ) {
  }
  object Config {
    val default = Config(Platform.charset.UTF8, "\n")
  }

  sealed trait NewlineStrategy {
    def apply(p: String): String
  }

  case class AsIsNewlineStrategy() extends NewlineStrategy {
    def apply(p: String): String = p
  }

  sealed trait HilightStrategy {
    def standard(p: String): String
    def error(p: String): String
    def warning(p: String): String
    def prompt(p: String): String
  }

  case class NoneHilightStrategy() extends HilightStrategy {
    def standard(p: String): String = p
    def error(p: String): String = p
    def warning(p: String): String = p
    def prompt(p: String): String = p
  }

  case class ColorHilightStrategy() extends HilightStrategy {
    def standard(p: String): String = p
    def error(p: String): String = Console.BLACK_B + Console.RED + Console.BOLD + p + Console.RESET
    def warning(p: String): String = Console.BLACK_B + Console.YELLOW + p + Console.RESET
    def prompt(p: String): String = Console.BOLD + p + Console.RESET
  }

  def create(env: Environment): ConsoleManager = {
    val config = ConsoleManager.Config(env.charset, env.newline)
    create(config)
  }

  def create(config: Config): ConsoleManager = new ConsoleManager(
    config,
    AsIsNewlineStrategy(),
    NoneHilightStrategy()
  )

  def createColorHilight(env: Environment): ConsoleManager = {
    val config = ConsoleManager.Config(env.charset, env.newline)
    createColorHilight(config)
  }

  def createColorHilight(config: Config): ConsoleManager = new ConsoleManager(
    config,
    AsIsNewlineStrategy(),
    ColorHilightStrategy()
  )
}
