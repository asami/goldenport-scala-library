package org.goldenport.cli

import java.io.File
import java.net.URL
import java.nio.charset.Charset
import java.math.MathContext
import java.util.{Locale, TimeZone, Currency, ResourceBundle}
import com.typesafe.config.{Config => HoconConfig, ConfigFactory}
import org.goldenport.context.DateTimeContext
import org.goldenport.i18n.I18NContext
import org.goldenport.i18n.CalendarFormatter
import org.goldenport.i18n.EmptyResourceBundle
import org.goldenport.i18n.StringFormatter
import org.goldenport.hocon.RichConfig
import org.goldenport.hocon.HoconUtils
import org.goldenport.log.{LogConfig, LogLevel}
import org.goldenport.recorder.{Recorder, StandardRecorder}
import org.goldenport.matrix.{INumericalOperations, GoldenportNumericalOperations}

/*
 * @since   Oct.  4, 2018
 *  version Feb.  8, 2019
 *  version Mar. 24, 2019
 *  version May. 19, 2019
 *  version Aug.  4, 2019
 *  version Sep. 25, 2019
 *  version Oct. 27, 2019
 *  version Jan. 20, 2020
 *  version Feb. 26, 2020
 *  version Mar. 12, 2020
 *  version Apr. 10, 2020
 *  version May. 16, 2020
 *  version Jan. 24, 2021
 *  version Oct.  2, 2021
 *  version Feb. 28, 2022
 *  version Apr.  4, 2022
 *  version Jan. 30, 2023
 * @version Jul. 22, 2023
 * @author  ASAMI, Tomoharu
 */
case class Config(
  dateTimeContext: DateTimeContext,
  i18n: I18NContext,
  mathContext: MathContext,
  homeDirectory: Option[File],
  workDirectory: Option[File],
  tmpDirectory: Option[File],
  projectDirectory: Option[File],
  log: LogConfig,
  numericalOperations: INumericalOperations,
  properties: RichConfig
) {
  def charset: Charset = i18n.charset
  def charsetInputFile = i18n.charsetInputFile
  def charsetOutputFile = i18n.charsetOutputFile
  def charsetConsole = i18n.charsetConsole
  def newline: String = i18n.newline
  def locale: Locale = i18n.locale
  def timezone: TimeZone = i18n.timezone
  def logLevel: Option[LogLevel] = log.level
  def consoleCharset: Charset = charset // XXX

  def outputDirectory: File = projectDirectory orElse workDirectory getOrElse new File(".")

  lazy val recorder: Recorder = new StandardRecorder() // TODO upper layer

  def withLogLevel(p: LogLevel) = copy(log = log.withLogLevel(p))

  def makeConfig(key: String): HoconConfig = properties.asConfig(key)
}

object Config {
  val default = build()
  val c = default.copy(i18n = I18NContext.c)
  val empty = _create(ConfigFactory.load())

  def build(): Config = {
    val hocon = _build()
    _create(hocon)
  }

  def build(args: Array[String]): Config = {
    val hocon = _build()
    // TODO args
    _create(hocon)
  }

  def build(appname: String): Config = {
    val hocon = _build(appname)
    _create(hocon)
  }

  def build(appname: String, args: Array[String]): Config = {
    val hocon0 = _build(appname)
    val specparams = List(
      spec.Parameter.property("mode")
    )
    val parser = spec.Request(specparams)
    val req = Request(appname)
    val (parsed, remainder) = parser.parse(req, args)
    val props = HoconUtils.createHocon(parsed.toPropertyMap)
    val hocon = props.withFallback(hocon0)
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

  val DEFAULT_RESOURCE_BUNDLE_NAME = "Resources"

  private def _create(hocon: HoconConfig): Config = _create(DEFAULT_RESOURCE_BUNDLE_NAME, hocon)

  private def _create(resourcename: String, hocon: HoconConfig): Config = {
    // TODO hocon
    val charset = Charset.defaultCharset()
    val newline = System.lineSeparator()
    val locale = Locale.getDefault()
    val timezone = TimeZone.getDefault()
    val dateTimeContext = DateTimeContext.now(timezone)
    val mathcontext = MathContext.UNLIMITED // Scala default: MathContext.DECIMAL128
    val currency = Currency.getInstance(locale)
    val calenderformatters = CalendarFormatter.Factory.default
    val stringformatter = StringFormatter.default
    val bundle = EmptyResourceBundle
    val homedir = Option(System.getProperty("user.home")).map(x => new File(x))
    val workdir = Option(System.getProperty("user.dir")).map(x => new File(x))
    val tmpdir = Option(System.getProperty("java.io.tmpdir")).map(x => new File(x))
    val projectdir = None // TODO
    Config(
      dateTimeContext,
      I18NContext(
        charset,
        None,
        None,
        None,
        newline,
        locale,
        timezone,
        currency,
        calenderformatters,
        stringformatter,
        bundle
      ),
      mathcontext,
      homedir,
      workdir,
      tmpdir,
      projectdir,
      LogConfig.empty,
      GoldenportNumericalOperations,
      RichConfig(hocon)
    )
  }
}
