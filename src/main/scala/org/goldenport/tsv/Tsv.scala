package org.goldenport.tsv

import scalaz._, Scalaz._
import scalaz.stream._
import org.goldenport.RAISE
import org.goldenport.matrix._
import org.goldenport.values.NumberRange

/*
 * @since   Jul.  7, 2019
 *  version Aug. 24, 2019
 *  version Sep. 16, 2019
 *  version Feb. 26, 2020
 * @version Oct. 18, 2024
 * @author  ASAMI, Tomoharu
 */
case class Tsv(matrix: VectorRowColumnMatrix[String]) extends IMatrix[String] {
  def apply(x: Int, y: Int): String = RAISE.notImplementedYetDefect
  def width: Int = RAISE.notImplementedYetDefect
  def height: Int = RAISE.notImplementedYetDefect
  override def rowIterator: Iterator[Vector[String]] = matrix.rowIterator
  override def columnIterator: Iterator[Vector[String]] = matrix.columnIterator
  def select(p: NumberRange): IMatrix[String] = RAISE.notImplementedYetDefect
  def filter(p: NumberRange): IMatrix[String] = RAISE.notImplementedYetDefect
  def toDoubleMatrix: IMatrix[Double] = RAISE.notImplementedYetDefect
  def makeDoubleMatrix: IMatrix[Double] = RAISE.notImplementedYetDefect
  def appendRow(ps: Seq[String]): IMatrix[String] = RAISE.unsupportedOperationFault
  def appendRows(ps: IMatrix[String]): IMatrix[String] = RAISE.unsupportedOperationFault
  def appendColumn(ps: Seq[String]): IMatrix[String] = RAISE.unsupportedOperationFault
  def appendColumns(ps: IMatrix[String]): IMatrix[String] = RAISE.unsupportedOperationFault
  def transpose: IMatrix[String] = RAISE.notImplementedYetDefect
  def map[A](p: String => A): IMatrix[A] = RAISE.notImplementedYetDefect
}
