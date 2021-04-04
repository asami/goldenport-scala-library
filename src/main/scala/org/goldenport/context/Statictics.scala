package org.goldenport.context

import org.goldenport.config.Config
import org.goldenport.recorder.ForwardRecorder

/*
 * @since   Mar. 13, 2021
 * @version Mar. 13, 2021
 * @author  ASAMI, Tomoharu
 */
sealed trait Statictics extends Incident {
}

case class Staticticses(
  staticticss: Vector[Statictics] = Vector.empty
) {
}
object Staticticses {
  val empty = Staticticses()
}
