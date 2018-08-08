package org.goldenport.util

/*
 * @since   Jul. 16, 2018
 * @version Aug.  5, 2018
 * @author  ASAMI, Tomoharu
 */
object VectorUtils {
  def buildTupleVector[T](fixed: Seq[(String, T)], options: Seq[(String, Option[T])]): Vector[(String, T)] =
    SeqUtils.buildTupleVector(fixed, options)

  def buildTupleVector[T](p: (String, Option[T]), ps: (String, Option[T])*): Vector[(String, T)] =
    SeqUtils.buildTupleVector(p +: ps)

  def buildTupleVector[T](options: Seq[(String, Option[T])]): Vector[(String, T)] =
    SeqUtils.buildTupleVector(options)
}
