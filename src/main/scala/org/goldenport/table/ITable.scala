package org.goldenport.table

import org.goldenport.RAISE
import org.goldenport.matrix.{IMatrix, VectorRowColumnMatrix}
import org.goldenport.extension.Showable

/*
 * @since   Feb. 11, 2019
 *  version Jun. 23, 2019
 *  version Jul. 28, 2019
 * @version Aug.  3, 2019
 * @author  ASAMI, Tomoharu
 */
trait ITable extends Showable {
  def width: Int
  def height: Int
  def matrix: IMatrix[Any]
}

case class Table(matrix: IMatrix[Any], schema: Option[ISchema]) extends ITable {
  def width = matrix.width
  def height = matrix.height
  def print: String = toString
  def display: String = print
  def show: String = display
}

object Table {
  def apply(data: Seq[Seq[Any]], schema: Option[ISchema]): Table =
    Table(VectorRowColumnMatrix.create(data), schema)
}
