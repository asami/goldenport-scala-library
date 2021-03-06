package org.goldenport.recorder

/*
 * Derived from ForwardingRecorder.
 * 
 * Provides recording facility
 *
 * Logger, Messager, Reporter
 *
 * @since   Oct. 29, 2008
 *  version Nov.  5, 2011
 *  version Dec.  9, 2011
 *  version Jan. 23, 2012
 *  version Nov.  8, 2012
 *  version Dec. 22, 2019
 *  version Jan.  4, 2020
 * @version Mar.  3, 2020
 * @author  ASAMI, Tomoharu
 */
trait Recordable {
  private var _recorder: Recorder = NullRecorder

  protected def set_Recorder(p: Recorder) {
    _recorder = p
  }

  protected final def record_error(ex: Throwable) {
    _recorder.error(ex)
  }

  protected final def record_error(message: String) {
    _recorder.error(message)
  }

  protected final def record_error(ex: Throwable, message: String) {
    _recorder.error(ex, message)
  }

  protected final def record_warning(message: String) {
    _recorder.warning(message)
  }

  protected final def record_info(message: String) {
    _recorder.info(message)
  }

  protected final def record_debug(message: => String) {
    _recorder.debug(message)
  }

  protected final def record_trace(message: => String) {
    _recorder.trace(message)
  }

  protected final def record_messageC(message: String) {
    _recorder.messageC(message)
  }

  protected final def record_message(message: String) {
    _recorder.message(message)
  }

  protected final def record_message() {
    _recorder.message()
  }

  protected final def record_report(message: String) {
    _recorder.report(message)
  }
}
