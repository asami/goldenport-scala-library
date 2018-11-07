package org.goldenport.cli

import java.io.File
import java.net.URL
import java.nio.charset.Charset
import java.util.{Locale, TimeZone}
import com.typesafe.config.{Config => HoconConfig, ConfigFactory}
import org.goldenport.util.HoconUtils.RichConfig
import org.goldenport.log.LogLevel

/*
 * @since   Oct.  4, 2018
 * @version Oct. 10, 2018
 * @author  ASAMI, Tomoharu
 */
case class Config(
  charset: Charset,
  newline: String,
  locale: Locale,
  timezone: TimeZone,
  logLevel: LogLevel,
  properties: RichConfig
) {
  def withLogLevel(p: LogLevel) = copy(logLevel = p)
}

object Config {
  val default = build()
  val empty = _create(ConfigFactory.load())

  def build(): Config = {
    val hocon = _build()
    _create(hocon)
  }

  def build(appname: String): Config = {
    val hocon = _build(appname)
    _create(hocon)
  }

  def build(appname: String, file: File): Config = {
    val base = _build(appname)
    val hocon = base.withFallback(ConfigFactory.parseFile(file))
    _create(hocon)
  }

  def build(appname: String, url: URL): Config = {
    val base = _build(appname)
    val hocon = base.withFallback(ConfigFactory.parseURL(url))
    _create(hocon)
  }

  private def _build(): HoconConfig = {
    val base = ConfigFactory.load()
    val homefw = _load_home("goldenport")
    val currentfw = _load_current("goldenport")
    Vector(homefw, currentfw)./:(base)((z, x) => x.map(z.withFallback).getOrElse(z))
  }

  private def _build(appname: String): HoconConfig = {
    val base = ConfigFactory.load()
    val homefw = _load_home("goldenport")
    val homeapp = _load_home(appname)
    val currentfw = _load_current("goldenport")
    val currentapp = _load_current(appname)
    Vector(homefw, homeapp, currentfw, currentapp)./:(base)((z, x) => x.map(z.withFallback).getOrElse(z))
  }

  private def _load_home(p: String): Option[HoconConfig] =
    for {
      filename <- Option(System.getProperty("user.home"))
      file <- _get_file(filename, p)
    } yield {
      ConfigFactory.parseFile(file)
    }

  private def _load_current(p: String): Option[HoconConfig] =
    for {
      filename <- Option(System.getProperty("user.dir"))
      file <- _get_file(filename, p)
    } yield {
      ConfigFactory.parseFile(file)
    }

  private def _get_file(dir: String, file: String): Option[File] = {
    val f = new File(new File(dir), file)
    if (f.exists)
      Some(f)
    else
      None
  }

  private def _create(hocon: HoconConfig): Config = {
    // TODO hocon
    val charset = Charset.defaultCharset()
    val newline = System.lineSeparator()
    val locale = Locale.getDefault()
    val timezone = TimeZone.getDefault()
    Config(
      charset,
      newline,
      locale,
      timezone,
      LogLevel.Warn,
      RichConfig(hocon)
    )
  }
}
