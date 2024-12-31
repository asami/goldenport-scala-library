package org.goldenport.matrix

import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.util.{AnyRefUtils, StringUtils}
import org.goldenport.collection.NonEmptyVector
import org.goldenport.collection.NonEmptyVector
import org.goldenport.parser.ParseResult

/*
 * @since   Sep. 16, 2019
 *  version Jan.  9, 2020
 *  version Mar.  1, 2021
 * @version Nov.  1, 2024
 * @author  ASAMI, Tomoharu
 */
object Matrix {
  def createDouble(ps: Seq[Seq[Double]]): IMatrix[Double] = VectorRowColumnMatrix.create(ps)

  def vectorVerticalFill[T](count: Int, value: T): IMatrix[T] =
    VectorColumnRowMatrix(Vector(Vector.fill(count)(value)))

  def horizontalConcatenate[T](ps: NonEmptyVector[IMatrix[T]]): IMatrix[T] =
    ps.tailVector./:(ps.head)(_ appendColumns _)

  def parseDouble(p: String): ParseResult[IMatrix[Double]] = ParseResult {
    val a = Strings.tolines(p).filter(Strings.notblankp)
    val b = a.map(x => Strings.totokens(x).map(_.toDouble))
    VectorRowColumnMatrix.create(b)
  }
}
