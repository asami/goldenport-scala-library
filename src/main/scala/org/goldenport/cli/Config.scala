package org.goldenport.cli

import java.io.File
import java.net.URL
import java.nio.charset.Charset
import java.math.MathContext
import java.util.{Locale, TimeZone, Currency, ResourceBundle}
import com.typesafe.config.{Config => HoconConfig, ConfigFactory}
import org.goldenport.context.ContextFoundation
import org.goldenport.context.DateTimeContext
import org.goldenport.context.FormatContext
import org.goldenport.context.RandomContext
import org.goldenport.i18n.I18NContext
import org.goldenport.i18n.CalendarFormatter
import org.goldenport.i18n.EmptyResourceBundle
import org.goldenport.i18n.StringFormatter
import org.goldenport.hocon.RichConfig
import org.goldenport.hocon.HoconUtils
import org.goldenport.notification.NotificationContext
import org.goldenport.observability.ObservabilityContext
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
 *  version Jul. 22, 2023
 *  version Oct. 14, 2024
 *  version Apr. 28, 2025
 * @version May. 11, 2025
 * @author  ASAMI, Tomoharu
 */
case class Config(
  contextFoundation: ContextFoundation,
  homeDirectory: Option[File],
  workDirectory: Option[File],
  tmpDirectory: Option[File],
  projectDirectory: Option[File],
  log: LogConfig,
  numericalOperations: INumericalOperations,
  properties: RichConfig
) extends ContextFoundation.Holder {
  def i18n = i18nContext
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

  lazy val recorder: Recorder = new StandardRecorder(
    observabilityContext,
    notificationContext
  )

  def withI18NContext(p: I18NContext) = copy(contextFoundation = contextFoundation.withI18NContext(p))
  def withLogLevel(p: LogLevel) = copy(log = log.withLogLevel(p))

  def makeConfig(key: String): HoconConfig = properties.asConfig(key)
}

object Config {
  type ParseResult[T] = ConfigurationParseState.Result[T]

  val DEFAULT_RESOURCE_BUNDLE_NAME = "Resources"
  val PROP_CHARSET = "charset"
  val PROP_NEWLINE = "newline"
  val PROP_LOCALE = "locale"
  val PROP_TIMEZONE = "timezone"

  case class Parameters(
    charset: Option[Charset] = None,
    lineSeparator: Option[String] = None,
    locale: Option[Locale] = None,
    dateTimeContext: Option[DateTimeContext] = None
  ) {
    def timezone: Option[TimeZone] = dateTimeContext.map(_.timezone)
  }
  object Parameters {
    val empty = Parameters()
  }

  val default = build()
  val c = default.withI18NContext(I18NContext.c)
  val empty = _create(ConfigFactory.load())

  def build(): Config = {
    val hocon = _build()
    _create(hocon)
  }

  def parse(p: ConfigurationParseState): ParseResult[Config] = {
    val hocon = _build()
    val state = p.complement(hocon)
    _parse(state)
  }

  // def parse(args: Array[String]): ParseResult[Config] = {
  //   val hocon = _build()
  //   val state = ConfigurationParseState(args, hocon)
  //   _parse(state)
  // }

  // def build0(args: Array[String]): (Array[String], Config) = {
  //   val hocon1 = _build()
  //   val (args1, hocon2) = _build_hocon(args)
  //   val hocon = hocon2.withFallback(hocon1)
  //   (args1, _create(hocon))
  // }

  // private def _build_hocon(args: Array[String]): (Array[String], HoconConfig) = {
  //   case class Z(
  //     props: Map[String, String] = Map.empty,
  //     candidate: Option[String] = None
  //   ) {
  //     def r = (remainder, HoconUtils.createHocon(props))

  //     def +(rhs: String) =
  //       if (rhs.startsWith("-")) {
  //         copy(candidate = Some(rhs.dropWhile(_ == '-')))
  //       } else {
  //         candidate match {
  //           case Some(s) => copy(props = props + (s -> rhs), candidate = None)
  //           case None => this
  //         }
  //       }
  //   }
  //   args.foldLeft(Z())(_+_).r
  // }

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

  def buildJaJp(): Config = parseJaJp(ConfigurationParseState.empty).result

  def parseJaJp(p: ConfigurationParseState): ParseResult[Config] = {
    val hocon = _build()
    val state = p.complement(hocon)
    val params = Parameters(
      Some(I18NContext.CHARSET_UTF8),
      Some(I18NContext.LINESEPARATOR_LF),
      Some(I18NContext.LOCALE_JA_JP),
      Some(DateTimeContext.nowJst())
    )
    _parse(state, params)
  }

  private def _parse(state: ConfigurationParseState, params: Parameters): ParseResult[Config] =
    _parse(DEFAULT_RESOURCE_BUNDLE_NAME, state, params)

  def buildEnUs(): Config = parseEnUs(ConfigurationParseState.empty).result

  def parseEnUs(p: ConfigurationParseState): ParseResult[Config] = {
    val hocon = _build()
    val state = p.complement(hocon)
    val params = Parameters(
      Some(I18NContext.CHARSET_UTF8),
      Some(I18NContext.LINESEPARATOR_LF),
      Some(I18NContext.LOCALE_EN_US),
      Some(DateTimeContext.nowEst())
    )
    _parse(state, params)
  }

  def buildEnGb(): Config = parseEnGb(ConfigurationParseState.empty).result

  def parseEnGb(p: ConfigurationParseState): ParseResult[Config] = {
    val hocon = _build()
    val state = p.complement(hocon)
    val params = Parameters(
      Some(I18NContext.CHARSET_UTF8),
      Some(I18NContext.LINESEPARATOR_LF),
      Some(I18NContext.LOCALE_EN_GB),
      Some(DateTimeContext.nowGmt())
    )
    _parse(state, params)
  }

  def buildDeDe(): Config = parseDeDe(ConfigurationParseState.empty).result

  def parseDeDe(p: ConfigurationParseState): ParseResult[Config] = {
    val hocon = _build()
    val state = p.complement(hocon)
    // TODO args
    val params = Parameters(
      Some(I18NContext.CHARSET_UTF8),
      Some(I18NContext.LINESEPARATOR_LF),
      Some(I18NContext.LOCALE_DE_DE),
      Some(DateTimeContext.nowEuropeBerlin())
    )
    _parse(state, params)
  }

  def buildDeCh(): Config = parseDeCh(ConfigurationParseState.empty).result

  def parseDeCh(p: ConfigurationParseState): ParseResult[Config] = {
    val hocon = _build()
    val state = p.complement(hocon)
    val params = Parameters(
      Some(I18NContext.CHARSET_UTF8),
      Some(I18NContext.LINESEPARATOR_LF),
      Some(I18NContext.LOCALE_DE_CH),
      Some(DateTimeContext.nowEuropeZurich())
    )
    _parse(state, params)
  }

  private def _build(): HoconConfig = {
    val base = ConfigFactory.load()
    val homefw = _load_home("goldenport")
    val currentfw = _load_current("goldenport")
    Vector(homefw, currentfw).foldLeft(base)((z, x) => x.map(z.withFallback).getOrElse(z))
  }

  private def _build(appname: String): HoconConfig = {
    val base = ConfigFactory.load()
    val homefw = _load_home("goldenport")
    val homeapp = _load_home(appname)
    val currentfw = _load_current("goldenport")
    val currentapp = _load_current(appname)
    Vector(homefw, homeapp, currentfw, currentapp).foldLeft(base)((z, x) => x.map(z.withFallback).getOrElse(z))
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

  private def _create(hocon: HoconConfig): Config = _create(DEFAULT_RESOURCE_BUNDLE_NAME, hocon, Parameters.empty)

  private def _create(hocon: HoconConfig, params: Parameters): Config = _create(DEFAULT_RESOURCE_BUNDLE_NAME, hocon, params)

  private def _create(resourcename: String, hocon: HoconConfig, params: Parameters): Config =
    _parse(resourcename, ConfigurationParseState(hocon), params).result

  private def _parse(state: ConfigurationParseState): ParseResult[Config] =
    _parse(DEFAULT_RESOURCE_BUNDLE_NAME, state, Parameters.empty)

  private def _parse(resourcename: String, state: ConfigurationParseState, params: Parameters): ParseResult[Config] = {
    // TODO hocon
    val charset = params.charset getOrElse Charset.defaultCharset()
    val newline = params.lineSeparator getOrElse System.lineSeparator()
    val locale = params.locale getOrElse Locale.getDefault()
    val timezone = params.timezone getOrElse TimeZone.getDefault()
    val datetimecontext = params.dateTimeContext getOrElse DateTimeContext.now(timezone)
    val mathcontext = MathContext.UNLIMITED // Scala default: MathContext.DECIMAL128
    val currency = Currency.getInstance(locale)
    val calendarformatters = CalendarFormatter.Factory.default
    val stringformatter = StringFormatter.default
    val formatcontext = FormatContext.create(locale, timezone)
    val bundle = EmptyResourceBundle // TODO
    val homedir = Option(System.getProperty("user.home")).map(x => new File(x))
    val workdir = Option(System.getProperty("user.dir")).map(x => new File(x))
    val tmpdir = Option(System.getProperty("java.io.tmpdir")).map(x => new File(x))
    val projectdir = None // TODO
    val i18ncontext = I18NContext(
      charset,
      None,
      None,
      None,
      newline,
      locale,
      timezone,
      currency,
      calendarformatters,
      stringformatter,
      bundle
    )
    val observabilitycontext = ObservabilityContext.default // TODO
    val notificationcontext = NotificationContext.default // TODO
    val randomcontext = RandomContext.default // TODO
    val contextfoundation = ContextFoundation(
      mathcontext,
      i18ncontext,
      datetimecontext,
      formatcontext,
      observabilitycontext,
      notificationcontext,
      randomcontext
    )
    val r1 = LogConfig.parse(state)
    val logconfig = r1.result
    val hocon = r1.state.hocon
    val c = Config(
      contextfoundation,
      homedir,
      workdir,
      tmpdir,
      projectdir,
      logconfig,
      GoldenportNumericalOperations,
      RichConfig(hocon)
    )
    ConfigurationParseState.Result(r1.state, c)
  }
}
