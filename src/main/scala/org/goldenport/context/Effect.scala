package org.goldenport.context

import org.goldenport.config.Config
import org.goldenport.recorder.ForwardRecorder

/*
 * @since   Feb. 21, 2021
 * @version Feb. 21, 2021
 * @author  ASAMI, Tomoharu
 */
sealed trait Effect {
}

case class Effects(
  effects: Vector[Effect] = Vector.empty
) {
}
object Effects {
  val empty = Effects()
}
