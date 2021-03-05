package org.goldenport.console

import java.io._

/*
 * @since   Jan. 10, 2021
 * @version Jan. 10, 2021
 * @author  ASAMI, Tomoharu
 */
trait ConsoleDevice {
}

object ConsoleDevice {
}

class SystemConsoleDevice(config: ConsoleManager.Config) extends ConsoleDevice {
  private val _stdin = new InputStreamReader(System.in, config.charset)
  private val _stdout = new PrintWriter(new OutputStreamWriter(System.out, config.charset), true)
  private val _stderr = new PrintWriter(new OutputStreamWriter(System.err, config.charset), true)

  def read(): Int = _stdin.read()

  def print(p: String): Unit = {
    _stdout.print(p)
  }

  def println(p: String): Unit = {
    _stdout.println(p)
  }

  def error(p: String): Unit = {
    _stderr.print(p)
  }

  def errorln(p: String): Unit = {
    _stderr.println(p)
  }

  def warning(p: String): Unit = {
    _stderr.print(p)
  }

  def warningln(p: String): Unit = {
    _stderr.println(p)
  }

  def prompt(p: String): Unit = {
    _stdout.print(p)
    _stdout.flush()
  }
}
