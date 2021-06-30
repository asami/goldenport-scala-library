package org.goldenport.event

import org.goldenport.RAISE

/*
 * @since   May. 22, 2021
 * @version Jun. 14, 2021
 * @author  ASAMI, Tomoharu
 */
trait Call {
  def to: ObjectId
}

object Call {
  case class StandardCall(
    to: ObjectId
  ) extends Call {
  }

  def apply(to: ObjectId): Call = StandardCall(to)
}
