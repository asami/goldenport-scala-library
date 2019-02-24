package org.goldenport.table

import org.goldenport.RAISE
import org.goldenport.matrix.{IMatrix, VectorRowColumnMatrix}

/*
 * @since   Feb. 11, 2019
 * @version Feb. 12, 2019
 * @author  ASAMI, Tomoharu
 */
trait ITable {
}

case class Table(matrix: IMatrix[Any], schema: Option[ISchema]) extends ITable {
}

object Table {
  def apply(data: Seq[Seq[Any]], schema: Option[ISchema]): Table =
    Table(VectorRowColumnMatrix.create(data), schema)
}
