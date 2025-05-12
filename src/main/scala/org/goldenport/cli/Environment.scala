package org.goldenport.cli

import java.io._
import java.net.URL
import org.goldenport.monitor.Monitor
import org.goldenport.context._
import org.goldenport.log.LogContext
import Environment._

/*
 * @since   Oct.  5, 2018
 *  version Feb. 24, 2019
 *  version Mar.  2, 2019
 *  version Jan. 20, 2020
 *  version Mar.  8, 2020
 *  version May. 30, 2020
 *  version Nov. 23, 2020
 *  version Jan. 24, 2021
 *  version Mar. 27, 2021
 *  version Feb. 28, 2022
 *  version Mar.  6, 2022
 *  version Oct. 14, 2024
 * @version May. 10, 2025
 * @author  ASAMI, Tomoharu
 */
case class Environment(
  monitor: Monitor,
  config: Config,
  appEnvironment: AppEnvironment = NoneAppEnvironment
) {
  def recorder = config.recorder
  def contextFoundation = config.contextFoundation
  def charset = config.charset
  def charsetInputFile = config.charsetInputFile
  def charsetOutputFile = config.charsetOutputFile
  def charsetConsole = config.charsetConsole
  def newline = config.newline
  def locale = config.locale
  def timezone = config.timezone
  def consoleCharset = config.consoleCharset
  def homeDirectory: File = config.homeDirectory getOrElse monitor.userHome
  def workDirectory: File = config.workDirectory getOrElse monitor.userDir
  def tmpDirectory: File = config.tmpDirectory getOrElse monitor.tmpDir
  def getProjectDirectory: Option[File] = config.projectDirectory
  def outputDirectory = config.outputDirectory
  def toAppEnvironment[T <: AppEnvironment] = appEnvironment.asInstanceOf[T]

  def isPlatformWindows: Boolean = monitor.isPlatformWindows

  def formatContext: FormatContext = FormatContext.default // config.formatContext
  val dateTimeContext: DateTimeContext = DateTimeContext.now() // config.dateTimeContext

  def withAppEnvironment(p: AppEnvironment) = copy(appEnvironment = p)

  def setupAppEnvironment(f: Environment => AppEnvironment): Environment = {
    val a = f(this)
    withAppEnvironment(a)
  }

  def initialize(): Unit = {
    val url = config.log.confFile
    val level = config.log.level
    LogContext.init(url, level)
  }

  lazy val stdout: PrintWriter = new PrintWriter(new OutputStreamWriter(System.out, consoleCharset), true)
  lazy val stderr: PrintWriter = new PrintWriter(new OutputStreamWriter(System.err, consoleCharset), true)

  def printStdoutLn(p: String) {
    // TODO last ln
    stdout.println(p)
    stdout.flush()
  }

  def printStderrLn(p: String) {
    // TODO last ln
    stderr.println(p)
    stderr.flush()
  }

  def getAppResource(o: Object, path: String): Option[URL] = Option(o.getClass().getClassLoader().getResource(path))

  def getClassResource(o: Object, path: String): Option[URL] = Option(o.getClass().getResource(path))
}

object Environment {
  trait AppEnvironment
  case object NoneAppEnvironment extends AppEnvironment

  def create(): Environment = parse(Array())._2

  def create(appname: String, args: Array[String]): Environment = {
    val config = Config.build(appname, args)
    new Environment(Monitor.default, config)
  }

  def create(args: Array[String]): Environment = parse(args)._2

  def parse(args: Array[String]): (Array[String], Environment) = {
    val s = ConfigurationParseState(args)
    val a = Monitor.parse(s)
    val monitor = a.result
    val b = Config.parse(a.state)
    (b.toArgs, new Environment(monitor, b.result))
  }

  def createJaJp(): Environment = parseJaJp(Array())._2

  def parseJaJp(args: Array[String]): (Array[String], Environment) = {
    val s = ConfigurationParseState(args)
    val a = Monitor.parse(s)
    val monitor = a.result
    val b = Config.parseJaJp(a.state)
    (b.toArgs, new Environment(monitor, b.result))
  }

  def createEnUs(): Environment = parseEnUs(Array())._2

  def parseEnUs(args: Array[String]): (Array[String], Environment) = {
    val s = ConfigurationParseState(args)
    val a = Monitor.parse(s)
    val monitor = a.result
    val b = Config.parseEnUs(a.state)
    (b.toArgs, new Environment(monitor, b.result))
  }

  def createEnGb(): Environment = parseEnGb(Array())._2

  def parseEnGb(args: Array[String]): (Array[String], Environment) = {
    val s = ConfigurationParseState(args)
    val a = Monitor.parse(s)
    val monitor = a.result
    val b = Config.parseEnGb(a.state)
    (b.toArgs, new Environment(monitor, b.result))
  }

  def createDeDe(): Environment = parseDeDe(Array())._2

  def parseDeDe(args: Array[String]): (Array[String], Environment) = {
    val s = ConfigurationParseState(args)
    val a = Monitor.parse(s)
    val monitor = a.result
    val b = Config.parseDeDe(a.state)
    (b.toArgs, new Environment(monitor, b.result))
  }

  def createDeCh(): Environment = parseDeCh(Array())._2

  def parseDeCh(args: Array[String]): (Array[String], Environment) = {
    val s = ConfigurationParseState(args)
    val a = Monitor.parse(s)
    val monitor = a.result
    val b = Config.parseDeCh(a.state)
    (b.toArgs, new Environment(monitor, b.result))
  }

  trait EnvironmentExecutionContextBase extends ExecutionContextBase {
    def environment: Environment

    protected def forward_Recorder = environment.recorder

    def locale = environment.locale
    override def formatContext = environment.formatContext
    override def dateTimeContext = environment.dateTimeContext
  }
}
