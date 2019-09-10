package org.goldenport.matrix

import org.goldenport.RAISE

/*
 * @since   Aug. 27, 2019
 * @version Aug. 27, 2019
 * @author  ASAMI, Tomoharu
 */
object MatrixUtils {
  def xToXy(f: Double => Double)(in: Seq[Double]): IMatrix[Double] = {
    val a = in.map(x => Vector(x, f(x)))
    VectorRowColumnMatrix(a.toVector)
  }

  def xToLine(f: Double => Double)(domain: Seq[Double]): IMatrix[Double] = {
    val from = domain.min
    val to = domain.max
    val by = 0.1
    xToSequence(f)(from, to, by)
  }

  def xToSequence(f: Double => Double)(from: Double, to: Double, step: Double): IMatrix[Double] = {
    val a = for (i <- from to to by step) yield Vector(i, f(i))
    VectorRowColumnMatrix(a.toVector)
  }
}
