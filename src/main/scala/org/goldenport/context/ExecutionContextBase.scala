package org.goldenport.context

import java.math.MathContext
import org.goldenport.config.Config
import org.goldenport.recorder.ForwardRecorder

/*
 * @since   Jan. 23, 2021
 * @version Jul. 22, 2023
 * @author  ASAMI, Tomoharu
 */
trait ExecutionContextBase extends ForwardRecorder {
  def config: Config
  def mathContext: MathContext
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
