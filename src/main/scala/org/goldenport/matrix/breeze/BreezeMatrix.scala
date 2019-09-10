package org.goldenport.matrix.breeze

import breeze.linalg
import breeze.linalg.{Vector => _, _}
// import breeze.math
// import breeze.numerics
import breeze.numerics._
import org.goldenport.RAISE
import org.goldenport.matrix._

/*
 * @since   Feb. 10, 2019
 *  version Jun. 23, 2019
 *  version Jul. 16, 2019
 *  version Aug. 25, 2019
 * @version Sep.  9, 2019
 * @author  ASAMI, Tomoharu
 */
case class BreezeMatrix(matrix: Matrix[Double]) extends IMatrix[Double] {
  lazy val denseMatrix = matrix.toDenseMatrix

  def apply(x: Int, y: Int): Double = matrix(y, x)
  def width: Int = matrix.cols
  def height: Int = matrix.rows
  def rowIterator: Iterator[Vector[Double]] = RAISE.notImplementedYetDefect
  def columnIterator: Iterator[Vector[Double]] = RAISE.notImplementedYetDefect

  def appendRow(ps: Seq[Double]): IMatrix[Double] = RAISE.notImplementedYetDefect
  def appendRows(ps: IMatrix[Double]): BreezeMatrix = RAISE.notImplementedYetDefect

  def transpose: IMatrix[Double] = BreezeMatrix(matrix.t.inner)
  //
  def +(rhs: BreezeMatrix) = BreezeMatrix(matrix + rhs.matrix)
  def *(rhs: BreezeMatrix) = BreezeMatrix(matrix * rhs.matrix)
//  def dot(rhs: BreezeMatrix) = BreezeMatrix(denseMatrix dot rhs.denseMatrix)
  def *:*(rhs: BreezeMatrix) = BreezeMatrix(matrix *:* rhs.matrix)
  def /:/(rhs: BreezeMatrix) = BreezeMatrix(matrix /:/ rhs.matrix)
//  def <:<(rhs: BreezeMatrix) = BreezeMatrix(matrix <:< rhs.matrix)
//  def :==(rhs: BreezeMatrix) = BreezeMatrix(matrix :== rhs.matrix)
  def :+=(rhs: Double) = BreezeMatrix(matrix :+= rhs)
  def :*=(rhs: Double) = BreezeMatrix(matrix :*= rhs)
  def max: Double = linalg.max(matrix)
  def argmax: (Int, Int) = linalg.argmax(matrix)
  def inv: IMatrix[Double] = BreezeMatrix(linalg.inv(denseMatrix))
  def det: Double = linalg.det(denseMatrix)
  def rank: Int = linalg.rank(denseMatrix)
}

object BreezeMatrix {
  def create(p: IMatrix[Double]): BreezeMatrix = BreezeMatrix(
    DenseMatrix.tabulate(p.height, p.width)((y, x) => p(x, y))
  )
}
