package org.goldenport.cli

import java.io._
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
 * @version Mar. 27, 2021
 * @author  ASAMI, Tomoharu
 */
case class Environment(
  monitor: Monitor,
  config: Config,
  appEnvironment: AppEnvironment = NoneAppEnvironment
) {
  def recorder = config.recorder
  def charset = config.charset
  def newline = config.newline
  def locale = config.locale
  def timezone = config.timezone
  def consoleCharset = config.consoleCharset
  def homeDirectory: File = config.homeDirectory getOrElse monitor.userHome
  def workDirectory: File = config.workDirectory getOrElse monitor.userDir
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

  trait EnvironmentExecutionContextBase extends ExecutionContextBase {
    def environment: Environment

    protected def forward_Recorder = environment.recorder

    def locale = environment.locale
    def formatContext = environment.formatContext
    def dateTimeContext = environment.dateTimeContext // TODO base
  }
}
