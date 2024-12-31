package org.goldenport.matrix.breeze

import breeze.linalg
import breeze.linalg.{Vector => _, _}
// import breeze.math
// import breeze.numerics
import breeze.numerics._
import org.goldenport.RAISE
import org.goldenport.matrix._
import org.goldenport.values.NumberRange

/*
 * @since   Feb. 10, 2019
 *  version Jun. 23, 2019
 *  version Jul. 16, 2019
 *  version Aug. 25, 2019
 *  version Sep. 16, 2019
 *  version Feb. 26, 2020
 * @version Oct. 18, 2024
 * @author  ASAMI, Tomoharu
 */
case class BreezeMatrix(matrix: Matrix[Double]) extends IMatrix[Double] {
  lazy val denseMatrix = matrix.toDenseMatrix

  def apply(x: Int, y: Int): Double = matrix(y, x)
  def width: Int = matrix.cols
  def height: Int = matrix.rows
  def select(p: NumberRange): IMatrix[Double] = RAISE.notImplementedYetDefect
  def filter(p: NumberRange): IMatrix[Double] = RAISE.notImplementedYetDefect
  def toDoubleMatrix: IMatrix[Double] = this
  def makeDoubleMatrix: IMatrix[Double] = this

  def appendRow(ps: Seq[Double]): IMatrix[Double] = RAISE.notImplementedYetDefect
  def appendRows(ps: IMatrix[Double]): BreezeMatrix = RAISE.notImplementedYetDefect
  def appendColumn(ps: Seq[Double]): IMatrix[Double] = RAISE.unsupportedOperationFault
  def appendColumns(ps: IMatrix[Double]): IMatrix[Double] = RAISE.unsupportedOperationFault

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
  def map[A](p: Double => A): IMatrix[A] = RAISE.notImplementedYetDefect
}

object BreezeMatrix {
  def create(p: IMatrix[Double]): BreezeMatrix = BreezeMatrix(
    DenseMatrix.tabulate(p.height, p.width)((y, x) => p(x, y))
  )
}
