package org.goldenport.recorder

import org.goldenport.log.Loggable

/*
 * @since   Apr. 10, 2020
 * @version Apr. 10, 2020
 * @author  ASAMI, Tomoharu
 */
trait RecorderBase extends Recorder with Loggable {
  def error(ex: Throwable): Unit = error(ex.getMessage)

  def error(message: String): Unit = {
    log_error(message)
    out_Error(message)
    out_Report(message)
  }

  def error(ex: Throwable, message: String): Unit = error(message)

  def warning(message: String): Unit = {
    log_warn(message)
    out_Error(message)
    out_Report(message)
  }

  def info(message: String): Unit = {
    log_info(message)
    out_Standard(message)
    out_Report(message)
  }

  def debug(message: => String): Unit = {
    log_debug(message)
    out_Standard(message)
    out_Report(message)
  }

  def trace(message: => String): Unit = {
    log_trace(message)
    out_Standard(message)
    out_Report(message)
  }

  def message(message: String): Unit = out_Standard(message)

  def messageC(message: String): Unit = out_Standard(message)

  def message(): Unit = out_Standard("") // TODO

  def report(message: String): Unit = out_Report(message)

  protected def out_Standard(message: String): Unit
  protected def out_Error(message: String): Unit
  protected def out_Report(message: String): Unit
}
