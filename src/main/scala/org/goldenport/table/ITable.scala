package org.goldenport.table

import org.goldenport.RAISE
import org.goldenport.matrix.{IMatrix, VectorRowColumnMatrix}

/*
 * @since   Feb. 11, 2019
 * @version Jun. 23, 2019
 * @author  ASAMI, Tomoharu
 */
trait ITable {
  def print: String
  def display: String
  def show: String
}

case class Table(matrix: IMatrix[Any], schema: Option[ISchema]) extends ITable {
  def print: String = toString
  def display: String = print
  def show: String = display
}

object Table {
  def apply(data: Seq[Seq[Any]], schema: Option[ISchema]): Table =
    Table(VectorRowColumnMatrix.create(data), schema)
}
