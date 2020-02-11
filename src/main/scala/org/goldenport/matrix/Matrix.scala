package org.goldenport.matrix

import org.goldenport.RAISE
import org.goldenport.util.{AnyRefUtils, StringUtils}
import org.goldenport.collection.NonEmptyVector
import org.goldenport.values.NumberRange

/*
 * @since   Sep. 16, 2019
 * @version Jan.  9, 2020
 * @author  ASAMI, Tomoharu
 */
object Matrix {
  def createDouble(ps: Seq[Seq[Double]]): IMatrix[Double] = VectorRowColumnMatrix.create(ps)

  def vectorVerticalFill[T](count: Int, value: T): IMatrix[T] =
    VectorColumnRowMatrix(Vector(Vector.fill(count)(value)))

  def horizontalConcatenate[T](ps: NonEmptyVector[IMatrix[T]]): IMatrix[T] =
    ps.tail./:(ps.head)(_ appendColumns _)
}
