package org.goldenport.context

import org.goldenport.config.Config
import org.goldenport.recorder.ForwardRecorder

/*
 * @since   Jan. 23, 2021
 * @version Jan. 23, 2021
 * @author  ASAMI, Tomoharu
 */
trait ExecutionContextBase extends ForwardRecorder {
  def config: Config
  def dateTimeContext: DateTimeContext
  def formatContext: FormatContext

  // typesafe config
  // locale
  // encoding
  // timezone
  // datetime format
  // message resource
  // logger
}
