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
 * @version Dec. 22, 2019
 * @author  ASAMI, Tomoharu
 */
trait Recordable {
  private var _recorder = NullRecorder

  protected final def record_error(ex: Throwable) {
    _recorder.error(ex)
  }

  protected final def record_error(message: String, args: Any*) {
    _recorder.error(message, args: _*)
  }

  protected final def record_error(ex: Throwable, message: String, args: Any*) {
    _recorder.error(ex, message, args: _*)
  }

  protected final def record_warning(message: String, args: Any*) {
    _recorder.warning(message, args: _*)
  }

  protected final def record_info(message: String, args: Any*) {
    _recorder.info(message, args: _*)
  }

  protected final def record_debug(message: => String) {
    _recorder.debug(message)
  }

  protected final def record_trace(message: => String) {
    _recorder.trace(message)
  }

  protected final def record_messageC(message: String, args: Any*) {
    _recorder.messageC(message, args: _*)
  }

  protected final def record_message(message: String, args: Any*) {
    _recorder.message(message, args: _*)
  }

  protected final def record_message() {
    _recorder.message()
  }

  protected final def record_report(message: String, args: Any*) {
    _recorder.report(message, args: _*)
  }
}
