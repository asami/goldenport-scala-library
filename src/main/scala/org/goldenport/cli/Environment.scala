package org.goldenport.cli

import java.io._
import java.net.URL
import org.goldenport.monitor.Monitor
import org.goldenport.context._
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
 * @version Oct. 14, 2024
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

  def create(): Environment = create(Array())

  def create(appname: String, args: Array[String]): Environment = {
    val config = Config.build(appname, args)
    new Environment(Monitor.default, config)
  }

  def create(args: Array[String]): Environment = {
    val monitor = Monitor.create(args)
    val config = Config.build(args)
    new Environment(monitor, config)
  }

  def createJaJp(): Environment = createJaJp(Array())

  def createJaJp(args: Array[String]): Environment = {
    val monitor = Monitor.create(args)
    val config = Config.buildJaJp(args)
    new Environment(monitor, config)
  }

  def createEnUs(): Environment = createEnUs(Array())

  def createEnUs(args: Array[String]): Environment = {
    val monitor = Monitor.create(args)
    val config = Config.buildEnUs(args)
    new Environment(monitor, config)
  }

  def createEnGb(): Environment = createEnGb(Array())

  def createEnGb(args: Array[String]): Environment = {
    val monitor = Monitor.create(args)
    val config = Config.buildEnGb(args)
    new Environment(monitor, config)
  }

  def createDeDe(): Environment = createDeDe(Array())

  def createDeDe(args: Array[String]): Environment = {
    val monitor = Monitor.create(args)
    val config = Config.buildDeDe(args)
    new Environment(monitor, config)
  }

  def createDeCh(): Environment = createDeCh(Array())

  def createDeCh(args: Array[String]): Environment = {
    val monitor = Monitor.create(args)
    val config = Config.buildDeCh(args)
    new Environment(monitor, config)
  }

  trait EnvironmentExecutionContextBase extends ExecutionContextBase {
    def environment: Environment

    protected def forward_Recorder = environment.recorder

    def locale = environment.locale
    override def formatContext = environment.formatContext
    override def dateTimeContext = environment.dateTimeContext
  }
}
