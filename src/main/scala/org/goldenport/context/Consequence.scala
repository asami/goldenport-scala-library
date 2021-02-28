package org.goldenport.context

import org.goldenport.config.Config
import org.goldenport.recorder.ForwardRecorder

/*
 * See org.goldenport.record.v2.ConclusionResult.
 * 
 * @since   Feb. 21, 2021
 * @version Feb. 21, 2021
 * @author  ASAMI, Tomoharu
 */
sealed trait Consequence[T] {
}

case class SuccessConsequence[T](
  result: T,
  conclusion: Conclusion = Conclusion.Ok
) extends Consequence[T] {
}

case class ErrorConsequence[T](
  conclusion: Conclusion = Conclusion.InternalServerError
) {
}
