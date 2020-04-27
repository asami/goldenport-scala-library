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
 *  version Dec. 22, 2019
 * @version Mar.  3, 2020
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
  def error(message: String)
  def error(ex: Throwable, message: String)

  /**
   * message
   * log: warning
   * report
   */
  def warning(message: String)

  /**
   * information for console and log
   * 
   * message
   * log: info
   */
  def info(message: String)

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
  def message(message: String)
  // @deprecated
  def messageC(message: String)
  def message()

  /**
   * information for report only
   * 
   * log: info
   * report
   */
  def report(message: String)
}

trait NullRecorder extends Recorder {
  override def error(ex: Throwable) {
  }

  override def error(message: String) {
  }

  override def error(ex: Throwable, message: String) {
  }

  override def warning(message: String) {
  }

  override def info(message: String) {
  }

  override def debug(message: => String) {
  }

  override def trace(message: => String) {
  }

  override def messageC(message: String) {
  }

  override def message(message: String) {
  }

  override def message() {
  }

  override def report(message: String) {
  }
}

object NullRecorder extends NullRecorder
