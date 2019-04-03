package org.goldenport.log

/*
 * @since   Feb. 17, 2019
 * @version Feb. 25, 2019
 * @author  ASAMI, Tomoharu
 */
trait Loggable {
  protected def log_Location: LogMark.Location = LogMark.ObjectLocation(this)
  protected def log_Action: LogMark.Action = LogMark.ProcessingAction
  protected def log_Label: Option[String] = None

  private lazy val _log_mark = LogMark(log_Location, log_Action, log_Label)
  private def _log_mark_start = LogMark(log_Location, LogMark.StartAction, log_Label)
  private def _log_mark_end = LogMark(log_Location, LogMark.EndAction, log_Label)

  protected def log_Mark: LogMark = _log_mark

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

  protected final def log_debug_start(label: String, msg: => String) {
    LogContext.log(LogMark(log_Location, LogMark.StartAction, label), LogLevel.Debug, msg)
  }

  protected final def log_debug_end(label: String, msg: => String) {
    LogContext.log(LogMark(log_Location, LogMark.EndAction, label), LogLevel.Debug, msg)
  }

  protected final def log_trace(msg: => String) {
    LogContext.log(log_Mark, LogLevel.Trace, msg)
  }

  protected final def log_trace_start(msg: => String) {
    LogContext.log(_log_mark_start, LogLevel.Trace, msg)
  }

  protected final def log_trace_end(msg: => String) {
    LogContext.log(_log_mark_end, LogLevel.Trace, msg)
  }
}
