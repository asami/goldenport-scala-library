package org.goldenport.recorder

// import org.goldenport.parameter.GParameterRepository

/*
 * Derived from GRecorder.
 *
 * Provides recording facility
 *
 * Logger, Messager, Reporter
 *
 * <dl>
 *   <dt>Logger</dt>
 *   <dd>Execution log for developers.<dd>
 *   <dt>Messager</dt>
 *   <dd>Execution message for application operators.<dd>
 *   <dt>Reporter</dt>
 *   <dd>Application result report for application users.<dd>
 * </dl>
 *
 * @since   Oct. 28, 2008
 *  version Oct. 30, 2011
 *  version Jan. 23, 2012
 * @version Dec. 22, 2019
 * @author  ASAMI, Tomoharu
 */
trait Recorder {
  // protected final def setup_GRecorder(theParams: GParameterRepository) {
  // }

  def openRecorder() {
  }

  def closeRecorder() {
  }

  /**
   * message
   * log: error
   * report
   */
  def error(ex: Throwable)
  def error(message: String, args: Any*)
  def error(ex: Throwable, message: String, args: Any*)

  /**
   * message
   * log: warning
   * report
   */
  def warning(message: String, args: Any*)

  /**
   * information for console and log
   * 
   * message
   * log: info
   */
  def info(message: String, args: Any*)

  /**
   * log: debug
   */
  def debug(message: => String)

  /**
   * log: trace
   */
  def trace(message: => String)

  protected final def record_trace_block[T](label: String)(
      enter: => String)(body: => T): T = {
    trace("<enter> " + label + " <= " + enter)
    val r = body
    trace("<leave> " + label + " => " + r)
    r
  }

  /**
   * information for console message and report
   * 
   * message
   * log: info
   * report
   */
  def message(message: String, args: Any*)
  // @deprecated
  def messageC(message: String, args: Any*)
  def message()

  /**
   * information for report only
   * 
   * log: info
   * report
   */
  def report(message: String, args: Any*)
}

trait NullRecorder extends Recorder {
  override def error(ex: Throwable) {
  }

  override def error(message: String, args: Any*) {
  }

  override def error(ex: Throwable, message: String, args: Any*) {
  }

  override def warning(message: String, args: Any*) {
  }

  override def info(message: String, args: Any*) {
  }

  override def debug(message: => String) {
  }

  override def trace(message: => String) {
  }

  override def messageC(message: String, args: Any*) {
  }

  override def message(message: String, args: Any*) {
  }

  override def message() {
  }

  override def report(message: String, args: Any*) {
  }
}

object NullRecorder extends NullRecorder
