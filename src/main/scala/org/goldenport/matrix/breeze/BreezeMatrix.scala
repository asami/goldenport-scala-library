package org.goldenport.matrix.breeze

import breeze.linalg.{Matrix, DenseMatrix}
import org.goldenport.RAISE
import org.goldenport.matrix._

/*
 * @since   Feb. 10, 2019
 * @version Feb. 10, 2019
 * @author  ASAMI, Tomoharu
 */
case class BreezeMatrix(matrix: Matrix[Double]) extends IMatrix[Double] {
  def apply(x: Int, y: Int): Double = ???
  def width: Int = ???
  def height: Int = ???
  def rowIterator: Iterator[Vector[Double]] = ???
  def columnIterator: Iterator[Vector[Double]] = ???
}

object BreezeMatrix {
  def create(p: IMatrix[Double]): BreezeMatrix = BreezeMatrix(
    DenseMatrix.tabulate(p.height, p.width)((i, j) => p(i, j))
  )
}
