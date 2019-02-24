package org.goldenport.log

/*
 * @since   Feb. 17, 2019
 * @version Feb. 24, 2019
 * @author  ASAMI, Tomoharu
 */
trait Loggable {
  protected def log_Location: LogMark.Location = LogMark.ApplicationLocation
  protected def log_Action: LogMark.Action = LogMark.ProcessingAction
  protected def log_Label: Option[String] = None

  private lazy val _log_mark = log_Label.map(LogMark(log_Location, log_Action, _))

  protected def log_Mark: Option[LogMark] = _log_mark

  protected final def log_error(msg: String) {
    LogContext.log(log_Mark, LogLevel.Error, msg)
  }

  protected final def log_warn(msg: String) {
    LogContext.log(log_Mark, LogLevel.Warn, msg)
  }

  protected final def log_info(msg: String) {
    LogContext.log(log_Mark, LogLevel.Info, msg)
  }

  protected final def log_debug(msg: => String) {
    LogContext.log(log_Mark, LogLevel.Debug, msg)
  }

  protected final def log_start_debug(label: String, msg: => String) {
    LogContext.log(log_Mark, LogLevel.Debug, msg)
  }

  protected final def log_end_debug(label: String, msg: => String) {
    LogContext.log(log_Mark, LogLevel.Debug, msg)
  }

  protected final def log_trace(msg: => String) {
    LogContext.log(log_Mark, LogLevel.Trace, msg)
  }

  protected final def log_start_trace(msg: => String) {
    LogContext.log(log_Mark, LogLevel.Trace, msg)
  }

  protected final def log_end_trace(msg: => String) {
    LogContext.log(log_Mark, LogLevel.Trace, msg)
  }
}
