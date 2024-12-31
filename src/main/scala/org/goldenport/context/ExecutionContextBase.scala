package org.goldenport.context

import org.goldenport.i18n.I18NContext
import org.goldenport.config.Config
import org.goldenport.recorder.ForwardRecorder

/*
 * @since   Jan. 23, 2021
 *  version Jul. 22, 2023
 * @version Oct. 13, 2024
 * @author  ASAMI, Tomoharu
 */
trait ExecutionContextBase extends ForwardRecorder with ContextFoundation.Holder {
  def config: Config

  // typesafe config
  // locale
  // encoding
  // timezone
  // datetime format
  // message resource
  // logger
}
