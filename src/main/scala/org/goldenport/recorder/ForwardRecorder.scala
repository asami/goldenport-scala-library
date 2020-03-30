package org.goldenport.recorder

/*
 * @since   Mar.  3, 2020
 * @version Mar.  3, 2020
 * @author  ASAMI, Tomoharu
 */
trait ForwardRecorder extends Recorder {
  protected def forward_Recorder: Recorder

  /**
   * message
   * log: error
   * report
   */
  def error(ex: Throwable) = forward_Recorder.error(ex)
  def error(message: String) = forward_Recorder.error(message)
  def error(ex: Throwable, message: String) = forward_Recorder.error(ex, message)

  /**
   * message
   * log: warning
   * report
   */
  def warning(message: String) = forward_Recorder.warning(message)

  /**
   * information for console and log
   * 
   * message
   * log: info
   */
  def info(message: String) = forward_Recorder.info(message)

  /**
   * log: debug
   */
  def debug(message: => String) = forward_Recorder.debug(message)

  /**
   * log: trace
   */
  def trace(message: => String) = forward_Recorder.trace(message)

  /**
   * information for console message and report
   * 
   * message
   * log: info
   * report
   */
  def message(message: String) = forward_Recorder.message(message)
  // @deprecated
  def messageC(message: String) = forward_Recorder.messageC(message)
  def message() = forward_Recorder.message()

  /**
   * information for report only
   * 
   * log: info
   * report
   */
  def report(message: String) = forward_Recorder.report(message)
}
